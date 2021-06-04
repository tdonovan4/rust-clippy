use rustc_hir::{self as hir, intravisit, HirIdSet};
use rustc_lint::LateContext;
use rustc_middle::{hir::map::Map, ty};
use rustc_session::declare_tool_lint;

use clippy_utils::diagnostics::span_lint;
use clippy_utils::ty::type_is_unsafe_function;
use clippy_utils::{iter_input_pats, path_to_local};

declare_clippy_lint! {
    /// **What it does:** Checks for public functions that dereference raw pointer
    /// arguments but are not marked `unsafe`.
    ///
    /// **Why is this bad?** The function should probably be marked `unsafe`, since
    /// for an arbitrary raw pointer, there is no way of telling for sure if it is
    /// valid.
    ///
    /// **Known problems:**
    ///
    /// * It does not check functions recursively so if the pointer is passed to a
    /// private non-`unsafe` function which does the dereferencing, the lint won't
    /// trigger.
    /// * It only checks for arguments whose type are raw pointers, not raw pointers
    /// got from an argument in some other way (`fn foo(bar: &[*const u8])` or
    /// `some_argument.get_raw_ptr()`).
    ///
    /// **Example:**
    /// ```rust,ignore
    /// // Bad
    /// pub fn foo(x: *const u8) {
    ///     println!("{}", unsafe { *x });
    /// }
    ///
    /// // Good
    /// pub unsafe fn foo(x: *const u8) {
    ///     println!("{}", unsafe { *x });
    /// }
    /// ```
    pub NOT_UNSAFE_PTR_ARG_DEREF,
    correctness,
    "public functions dereferencing raw pointer arguments but not marked `unsafe`"
}

pub(super) fn check_fn(
    cx: &LateContext<'tcx>,
    kind: intravisit::FnKind<'tcx>,
    decl: &'tcx hir::FnDecl<'tcx>,
    body: &'tcx hir::Body<'tcx>,
    hir_id: hir::HirId,
) {
    let unsafety = match kind {
        intravisit::FnKind::ItemFn(_, _, hir::FnHeader { unsafety, .. }, _) => unsafety,
        intravisit::FnKind::Method(_, sig, _) => sig.header.unsafety,
        intravisit::FnKind::Closure => return,
    };

    check_raw_ptr(cx, unsafety, decl, body, hir_id);
}

pub(super) fn check_trait_item(cx: &LateContext<'tcx>, item: &'tcx hir::TraitItem<'_>) {
    if let hir::TraitItemKind::Fn(ref sig, hir::TraitFn::Provided(eid)) = item.kind {
        let body = cx.tcx.hir().body(eid);
        check_raw_ptr(cx, sig.header.unsafety, sig.decl, body, item.hir_id());
    }
}

fn check_raw_ptr(
    cx: &LateContext<'tcx>,
    unsafety: hir::Unsafety,
    decl: &'tcx hir::FnDecl<'tcx>,
    body: &'tcx hir::Body<'tcx>,
    hir_id: hir::HirId,
) {
    let expr = &body.value;
    if unsafety == hir::Unsafety::Normal && cx.access_levels.is_exported(hir_id) {
        let raw_ptrs = iter_input_pats(decl, body)
            .zip(decl.inputs.iter())
            .filter_map(|(arg, ty)| raw_ptr_arg(arg, ty))
            .collect::<HirIdSet>();

        if !raw_ptrs.is_empty() {
            let typeck_results = cx.tcx.typeck_body(body.id());
            let mut v = DerefVisitor {
                cx,
                ptrs: raw_ptrs,
                typeck_results,
            };

            intravisit::walk_expr(&mut v, expr);
        }
    }
}

fn raw_ptr_arg(arg: &hir::Param<'_>, ty: &hir::Ty<'_>) -> Option<hir::HirId> {
    if let (&hir::PatKind::Binding(_, id, _, _), &hir::TyKind::Ptr(_)) = (&arg.pat.kind, &ty.kind) {
        Some(id)
    } else {
        None
    }
}

struct DerefVisitor<'a, 'tcx> {
    cx: &'a LateContext<'tcx>,
    ptrs: HirIdSet,
    typeck_results: &'a ty::TypeckResults<'tcx>,
}

impl<'a, 'tcx> intravisit::Visitor<'tcx> for DerefVisitor<'a, 'tcx> {
    type Map = Map<'tcx>;

    fn visit_expr(&mut self, expr: &'tcx hir::Expr<'_>) {
        match expr.kind {
            hir::ExprKind::Call(f, args) => {
                let ty = self.typeck_results.expr_ty(f);

                if type_is_unsafe_function(self.cx, ty) {
                    for arg in args {
                        self.check_arg(arg);
                    }
                }
            },
            hir::ExprKind::MethodCall(_, _, args, _) => {
                let def_id = self.typeck_results.type_dependent_def_id(expr.hir_id).unwrap();
                let base_type = self.cx.tcx.type_of(def_id);

                if type_is_unsafe_function(self.cx, base_type) {
                    for arg in args {
                        self.check_arg(arg);
                    }
                }
            },
            hir::ExprKind::Unary(hir::UnOp::Deref, ptr) => self.check_arg(ptr),
            _ => (),
        }

        intravisit::walk_expr(self, expr);
    }

    fn nested_visit_map(&mut self) -> intravisit::NestedVisitorMap<Self::Map> {
        intravisit::NestedVisitorMap::None
    }
}

impl<'a, 'tcx> DerefVisitor<'a, 'tcx> {
    fn check_arg(&self, ptr: &hir::Expr<'_>) {
        if let Some(id) = path_to_local(ptr) {
            if self.ptrs.contains(&id) {
                span_lint(
                    self.cx,
                    NOT_UNSAFE_PTR_ARG_DEREF,
                    ptr.span,
                    "this public function dereferences a raw pointer but is not marked `unsafe`",
                );
            }
        }
    }
}
