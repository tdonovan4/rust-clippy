use crate::consts::constant;
use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::usage::mutated_variables;
use if_chain::if_chain;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefIdMap;
use rustc_hir::intravisit::{walk_expr, NestedVisitorMap, Visitor};
use rustc_hir::HirIdSet;
use rustc_hir::{Expr, ExprKind, QPath};
use rustc_lint::LateContext;
use rustc_middle::hir::map::Map;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks whether variables used within while loop condition
    /// can be (and are) mutated in the body.
    ///
    /// **Why is this bad?** If the condition is unchanged, entering the body of the loop
    /// will lead to an infinite loop.
    ///
    /// **Known problems:** If the `while`-loop is in a closure, the check for mutation of the
    /// condition variables in the body can cause false negatives. For example when only `Upvar` `a` is
    /// in the condition and only `Upvar` `b` gets mutated in the body, the lint will not trigger.
    ///
    /// **Example:**
    /// ```rust
    /// let i = 0;
    /// while i > 10 {
    ///     println!("let me loop forever!");
    /// }
    /// ```
    pub WHILE_IMMUTABLE_CONDITION,
    correctness,
    "variables used within while expression are not mutated in the body"
}

pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, cond: &'tcx Expr<'_>, expr: &'tcx Expr<'_>) {
    if constant(cx, cx.typeck_results(), cond).is_some() {
        // A pure constant condition (e.g., `while false`) is not linted.
        return;
    }

    let mut var_visitor = VarCollectorVisitor {
        cx,
        ids: HirIdSet::default(),
        def_ids: DefIdMap::default(),
        skip: false,
    };
    var_visitor.visit_expr(cond);
    if var_visitor.skip {
        return;
    }
    let used_in_condition = &var_visitor.ids;
    let no_cond_variable_mutated = if let Some(used_mutably) = mutated_variables(expr, cx) {
        used_in_condition.is_disjoint(&used_mutably)
    } else {
        return;
    };
    let mutable_static_in_cond = var_visitor.def_ids.iter().any(|(_, v)| *v);

    let mut has_break_or_return_visitor = HasBreakOrReturnVisitor {
        has_break_or_return: false,
    };
    has_break_or_return_visitor.visit_expr(expr);
    let has_break_or_return = has_break_or_return_visitor.has_break_or_return;

    if no_cond_variable_mutated && !mutable_static_in_cond {
        span_lint_and_then(
            cx,
            WHILE_IMMUTABLE_CONDITION,
            cond.span,
            "variables in the condition are not mutated in the loop body",
            |diag| {
                diag.note("this may lead to an infinite or to a never running loop");

                if has_break_or_return {
                    diag.note("this loop contains `return`s or `break`s");
                    diag.help("rewrite it as `if cond { loop { } }`");
                }
            },
        );
    }
}

struct HasBreakOrReturnVisitor {
    has_break_or_return: bool,
}

impl<'tcx> Visitor<'tcx> for HasBreakOrReturnVisitor {
    type Map = Map<'tcx>;

    fn visit_expr(&mut self, expr: &'tcx Expr<'_>) {
        if self.has_break_or_return {
            return;
        }

        match expr.kind {
            ExprKind::Ret(_) | ExprKind::Break(_, _) => {
                self.has_break_or_return = true;
                return;
            },
            _ => {},
        }

        walk_expr(self, expr);
    }

    fn nested_visit_map(&mut self) -> NestedVisitorMap<Self::Map> {
        NestedVisitorMap::None
    }
}

/// Collects the set of variables in an expression
/// Stops analysis if a function call is found
/// Note: In some cases such as `self`, there are no mutable annotation,
/// All variables definition IDs are collected
struct VarCollectorVisitor<'a, 'tcx> {
    cx: &'a LateContext<'tcx>,
    ids: HirIdSet,
    def_ids: DefIdMap<bool>,
    skip: bool,
}

impl<'a, 'tcx> VarCollectorVisitor<'a, 'tcx> {
    fn insert_def_id(&mut self, ex: &'tcx Expr<'_>) {
        if_chain! {
            if let ExprKind::Path(ref qpath) = ex.kind;
            if let QPath::Resolved(None, _) = *qpath;
            then {
                match self.cx.qpath_res(qpath, ex.hir_id) {
                    Res::Local(hir_id) => {
                        self.ids.insert(hir_id);
                    },
                    Res::Def(DefKind::Static, def_id) => {
                        let mutable = self.cx.tcx.is_mutable_static(def_id);
                        self.def_ids.insert(def_id, mutable);
                    },
                    _ => {},
                }
            }
        }
    }
}

impl<'a, 'tcx> Visitor<'tcx> for VarCollectorVisitor<'a, 'tcx> {
    type Map = Map<'tcx>;

    fn visit_expr(&mut self, ex: &'tcx Expr<'_>) {
        match ex.kind {
            ExprKind::Path(_) => self.insert_def_id(ex),
            // If there is any function/method call… we just stop analysis
            ExprKind::Call(..) | ExprKind::MethodCall(..) => self.skip = true,

            _ => walk_expr(self, ex),
        }
    }

    fn nested_visit_map(&mut self) -> NestedVisitorMap<Self::Map> {
        NestedVisitorMap::None
    }
}
