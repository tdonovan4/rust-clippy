use clippy_utils::diagnostics::{multispan_sugg, span_lint_and_then};
use clippy_utils::source::snippet;
use clippy_utils::ty::{is_type_diagnostic_item, match_type};
use clippy_utils::visitors::LocalUsedVisitor;
use clippy_utils::{paths, sugg};
use rustc_hir::{BorrowKind, Expr, ExprKind, Mutability, Pat, PatKind};
use rustc_lint::LateContext;
use rustc_middle::ty;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for iterating a map (`HashMap` or `BTreeMap`) and
    /// ignoring either the keys or values.
    ///
    /// **Why is this bad?** Readability. There are `keys` and `values` methods that
    /// can be used to express that don't need the values or keys.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```ignore
    /// for (k, _) in &map {
    ///     ..
    /// }
    /// ```
    ///
    /// could be replaced by
    ///
    /// ```ignore
    /// for k in map.keys() {
    ///     ..
    /// }
    /// ```
    pub FOR_KV_MAP,
    style,
    "looping on a map using `iter` when `keys` or `values` would do"
}

/// Checks for the `FOR_KV_MAP` lint.
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    pat: &'tcx Pat<'_>,
    arg: &'tcx Expr<'_>,
    body: &'tcx Expr<'_>,
    expr: &'tcx Expr<'_>,
) {
    let pat_span = pat.span;

    if let PatKind::Tuple(pat, _) = pat.kind {
        if pat.len() == 2 {
            let arg_span = arg.span;
            let (new_pat_span, kind, ty, mutbl) = match *cx.typeck_results().expr_ty(arg).kind() {
                ty::Ref(_, ty, mutbl) => match (&pat[0].kind, &pat[1].kind) {
                    (key, _) if pat_is_wild(cx, key, body) => (pat[1].span, "value", ty, mutbl),
                    (_, value) if pat_is_wild(cx, value, body) => (pat[0].span, "key", ty, Mutability::Not),
                    _ => return,
                },
                _ => return,
            };
            let mutbl = match mutbl {
                Mutability::Not => "",
                Mutability::Mut => "_mut",
            };
            let arg = match arg.kind {
                ExprKind::AddrOf(BorrowKind::Ref, _, expr) => expr,
                _ => arg,
            };

            if is_type_diagnostic_item(cx, ty, sym::hashmap_type) || match_type(cx, ty, &paths::BTREEMAP) {
                span_lint_and_then(
                    cx,
                    FOR_KV_MAP,
                    expr.span,
                    &format!("you seem to want to iterate on a map's {}s", kind),
                    |diag| {
                        let map = sugg::Sugg::hir(cx, arg, "map");
                        multispan_sugg(
                            diag,
                            "use the corresponding method",
                            vec![
                                (pat_span, snippet(cx, new_pat_span, kind).into_owned()),
                                (arg_span, format!("{}.{}s{}()", map.maybe_par(), kind, mutbl)),
                            ],
                        );
                    },
                );
            }
        }
    }
}

/// Returns `true` if the pattern is a `PatWild` or an ident prefixed with `_`.
fn pat_is_wild<'tcx>(cx: &LateContext<'tcx>, pat: &'tcx PatKind<'_>, body: &'tcx Expr<'_>) -> bool {
    match *pat {
        PatKind::Wild => true,
        PatKind::Binding(_, id, ident, None) if ident.as_str().starts_with('_') => {
            !LocalUsedVisitor::new(cx, id).check_expr(body)
        },
        _ => false,
    }
}
