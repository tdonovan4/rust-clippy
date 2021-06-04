use clippy_utils::diagnostics::span_lint;
use if_chain::if_chain;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_middle::ty;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for `offset(_)`, `wrapping_`{`add`, `sub`}, etc. on raw pointers to
    /// zero-sized types
    ///
    /// **Why is this bad?** This is a no-op, and likely unintended
    ///
    /// **Known problems:** None
    ///
    /// **Example:**
    /// ```rust
    /// unsafe { (&() as *const ()).offset(1) };
    /// ```
    pub ZST_OFFSET,
    correctness,
    "Check for offset calculations on raw pointers to zero-sized types"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, recv: &hir::Expr<'_>) {
    if_chain! {
        if let ty::RawPtr(ty::TypeAndMut { ty, .. }) = cx.typeck_results().expr_ty(recv).kind();
        if let Ok(layout) = cx.tcx.layout_of(cx.param_env.and(ty));
        if layout.is_zst();
        then {
            span_lint(cx, ZST_OFFSET, expr.span, "offset calculation on zero-sized value");
        }
    }
}
