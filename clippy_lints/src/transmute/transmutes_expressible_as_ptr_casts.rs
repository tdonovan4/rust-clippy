use super::utils::can_be_expressed_as_pointer_cast;
use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::sugg;
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::Ty;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:**Checks for transmutes that could be a pointer cast.
    ///
    /// **Why is this bad?** Readability. The code tricks people into thinking that
    /// something complex is going on.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// # let p: *const [i32] = &[];
    /// unsafe { std::mem::transmute::<*const [i32], *const [u16]>(p) };
    /// ```
    /// Use instead:
    /// ```rust
    /// # let p: *const [i32] = &[];
    /// p as *const [u16];
    /// ```
    pub TRANSMUTES_EXPRESSIBLE_AS_PTR_CASTS,
    complexity,
    "transmutes that could be a pointer cast"
}

/// Checks for `transmutes_expressible_as_ptr_casts` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    e: &'tcx Expr<'_>,
    from_ty: Ty<'tcx>,
    to_ty: Ty<'tcx>,
    args: &'tcx [Expr<'_>],
) -> bool {
    if can_be_expressed_as_pointer_cast(cx, e, from_ty, to_ty) {
        span_lint_and_then(
            cx,
            TRANSMUTES_EXPRESSIBLE_AS_PTR_CASTS,
            e.span,
            &format!(
                "transmute from `{}` to `{}` which could be expressed as a pointer cast instead",
                from_ty, to_ty
            ),
            |diag| {
                if let Some(arg) = sugg::Sugg::hir_opt(cx, &args[0]) {
                    let sugg = arg.as_ty(&to_ty.to_string()).to_string();
                    diag.span_suggestion(e.span, "try", sugg, Applicability::MachineApplicable);
                }
            },
        );
        true
    } else {
        false
    }
}
