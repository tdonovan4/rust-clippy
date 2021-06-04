use super::utils::derefs_to_slice;
use crate::methods::iter_nth_zero;
use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::ty::is_type_diagnostic_item;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for use of `.iter().nth()` (and the related
    /// `.iter_mut().nth()`) on standard library types with O(1) element access.
    ///
    /// **Why is this bad?** `.get()` and `.get_mut()` are more efficient and more
    /// readable.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let some_vec = vec![0, 1, 2, 3];
    /// let bad_vec = some_vec.iter().nth(3);
    /// let bad_slice = &some_vec[..].iter().nth(3);
    /// ```
    /// The correct use would be:
    /// ```rust
    /// let some_vec = vec![0, 1, 2, 3];
    /// let bad_vec = some_vec.get(3);
    /// let bad_slice = &some_vec[..].get(3);
    /// ```
    pub ITER_NTH,
    perf,
    "using `.iter().nth()` on a standard library type with O(1) element access"
}

pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &hir::Expr<'_>,
    iter_recv: &'tcx hir::Expr<'tcx>,
    nth_recv: &hir::Expr<'_>,
    nth_arg: &hir::Expr<'_>,
    is_mut: bool,
) {
    let mut_str = if is_mut { "_mut" } else { "" };
    let caller_type = if derefs_to_slice(cx, iter_recv, cx.typeck_results().expr_ty(iter_recv)).is_some() {
        "slice"
    } else if is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(iter_recv), sym::vec_type) {
        "Vec"
    } else if is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(iter_recv), sym::vecdeque_type) {
        "VecDeque"
    } else {
        iter_nth_zero::check(cx, expr, nth_recv, nth_arg);
        return; // caller is not a type that we want to lint
    };

    span_lint_and_help(
        cx,
        ITER_NTH,
        expr.span,
        &format!("called `.iter{0}().nth()` on a {1}", mut_str, caller_type),
        None,
        &format!("calling `.get{}()` is both faster and more readable", mut_str),
    );
}
