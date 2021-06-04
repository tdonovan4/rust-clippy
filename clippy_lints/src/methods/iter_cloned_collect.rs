use crate::methods::utils::derefs_to_slice;
use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::ty::is_type_diagnostic_item;
use if_chain::if_chain;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for the use of `.cloned().collect()` on slice to
    /// create a `Vec`.
    ///
    /// **Why is this bad?** `.to_vec()` is clearer
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let s = [1, 2, 3, 4, 5];
    /// let s2: Vec<isize> = s[..].iter().cloned().collect();
    /// ```
    /// The better use would be:
    /// ```rust
    /// let s = [1, 2, 3, 4, 5];
    /// let s2: Vec<isize> = s.to_vec();
    /// ```
    pub ITER_CLONED_COLLECT,
    style,
    "using `.cloned().collect()` on slice to create a `Vec`"
}

pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, expr: &hir::Expr<'_>, recv: &'tcx hir::Expr<'_>) {
    if_chain! {
        if is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(expr), sym::vec_type);
        if let Some(slice) = derefs_to_slice(cx, recv, cx.typeck_results().expr_ty(recv));
        if let Some(to_replace) = expr.span.trim_start(slice.span.source_callsite());

        then {
            span_lint_and_sugg(
                cx,
                ITER_CLONED_COLLECT,
                to_replace,
                "called `iter().cloned().collect()` on a slice to create a `Vec`. Calling `to_vec()` is both faster and \
                more readable",
                "try",
                ".to_vec()".to_string(),
                Applicability::MachineApplicable,
            );
        }
    }
}
