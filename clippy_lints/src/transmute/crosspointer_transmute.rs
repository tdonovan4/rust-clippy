use clippy_utils::diagnostics::span_lint;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

// FIXME: Merge this lint with USELESS_TRANSMUTE once that is out of the nursery.
declare_clippy_lint! {
    /// **What it does:** Checks for transmutes between a type `T` and `*T`.
    ///
    /// **Why is this bad?** It's easy to mistakenly transmute between a type and a
    /// pointer to that type.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust,ignore
    /// core::intrinsics::transmute(t) // where the result type is the same as
    ///                                // `*t` or `&t`'s
    /// ```
    pub CROSSPOINTER_TRANSMUTE,
    complexity,
    "transmutes that have to or from types that are a pointer to the other"
}

/// Checks for `crosspointer_transmute` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, e: &'tcx Expr<'_>, from_ty: Ty<'tcx>, to_ty: Ty<'tcx>) -> bool {
    match (&from_ty.kind(), &to_ty.kind()) {
        (ty::RawPtr(from_ptr), _) if from_ptr.ty == to_ty => {
            span_lint(
                cx,
                CROSSPOINTER_TRANSMUTE,
                e.span,
                &format!(
                    "transmute from a type (`{}`) to the type that it points to (`{}`)",
                    from_ty, to_ty
                ),
            );
            true
        },
        (_, ty::RawPtr(to_ptr)) if to_ptr.ty == from_ty => {
            span_lint(
                cx,
                CROSSPOINTER_TRANSMUTE,
                e.span,
                &format!(
                    "transmute from a type (`{}`) to a pointer to that type (`{}`)",
                    from_ty, to_ty
                ),
            );
            true
        },
        _ => false,
    }
}
