use clippy_utils::diagnostics::span_lint;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for transmutes that can't ever be correct on any
    /// architecture.
    ///
    /// **Why is this bad?** It's basically guaranteed to be undefined behaviour.
    ///
    /// **Known problems:** When accessing C, users might want to store pointer
    /// sized objects in `extradata` arguments to save an allocation.
    ///
    /// **Example:**
    /// ```ignore
    /// let ptr: *const T = core::intrinsics::transmute('x')
    /// ```
    pub WRONG_TRANSMUTE,
    correctness,
    "transmutes that are confusing at best, undefined behaviour at worst and always useless"
}

/// Checks for `wrong_transmute` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, e: &'tcx Expr<'_>, from_ty: Ty<'tcx>, to_ty: Ty<'tcx>) -> bool {
    match (&from_ty.kind(), &to_ty.kind()) {
        (ty::Float(_) | ty::Char, ty::Ref(..) | ty::RawPtr(_)) => {
            span_lint(
                cx,
                WRONG_TRANSMUTE,
                e.span,
                &format!("transmute from a `{}` to a pointer", from_ty),
            );
            true
        },
        _ => false,
    }
}
