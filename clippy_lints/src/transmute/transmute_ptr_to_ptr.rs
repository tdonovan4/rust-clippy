use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::sugg;
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for transmutes from a pointer to a pointer, or
    /// from a reference to a reference.
    ///
    /// **Why is this bad?** Transmutes are dangerous, and these can instead be
    /// written as casts.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let ptr = &1u32 as *const u32;
    /// unsafe {
    ///     // pointer-to-pointer transmute
    ///     let _: *const f32 = std::mem::transmute(ptr);
    ///     // ref-ref transmute
    ///     let _: &f32 = std::mem::transmute(&1u32);
    /// }
    /// // These can be respectively written:
    /// let _ = ptr as *const f32;
    /// let _ = unsafe{ &*(&1u32 as *const u32 as *const f32) };
    /// ```
    pub TRANSMUTE_PTR_TO_PTR,
    complexity,
    "transmutes from a pointer to a pointer / a reference to a reference"
}

/// Checks for `transmute_ptr_to_ptr` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    e: &'tcx Expr<'_>,
    from_ty: Ty<'tcx>,
    to_ty: Ty<'tcx>,
    args: &'tcx [Expr<'_>],
) -> bool {
    match (&from_ty.kind(), &to_ty.kind()) {
        (ty::RawPtr(_), ty::RawPtr(to_ty)) => {
            span_lint_and_then(
                cx,
                TRANSMUTE_PTR_TO_PTR,
                e.span,
                "transmute from a pointer to a pointer",
                |diag| {
                    if let Some(arg) = sugg::Sugg::hir_opt(cx, &args[0]) {
                        let sugg = arg.as_ty(cx.tcx.mk_ptr(*to_ty));
                        diag.span_suggestion(e.span, "try", sugg.to_string(), Applicability::Unspecified);
                    }
                },
            );
            true
        },
        _ => false,
    }
}
