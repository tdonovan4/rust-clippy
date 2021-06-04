use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::sugg;
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for transmutes from an integer to a float.
    ///
    /// **Why is this bad?** Transmutes are dangerous and error-prone, whereas `from_bits` is intuitive
    /// and safe.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// unsafe {
    ///     let _: f32 = std::mem::transmute(1_u32); // where x: u32
    /// }
    ///
    /// // should be:
    /// let _: f32 = f32::from_bits(1_u32);
    /// ```
    pub TRANSMUTE_INT_TO_FLOAT,
    complexity,
    "transmutes from an integer to a float"
}

/// Checks for `transmute_int_to_float` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    e: &'tcx Expr<'_>,
    from_ty: Ty<'tcx>,
    to_ty: Ty<'tcx>,
    args: &'tcx [Expr<'_>],
    const_context: bool,
) -> bool {
    match (&from_ty.kind(), &to_ty.kind()) {
        (ty::Int(_) | ty::Uint(_), ty::Float(_)) if !const_context => {
            span_lint_and_then(
                cx,
                TRANSMUTE_INT_TO_FLOAT,
                e.span,
                &format!("transmute from a `{}` to a `{}`", from_ty, to_ty),
                |diag| {
                    let arg = sugg::Sugg::hir(cx, &args[0], "..");
                    let arg = if let ty::Int(int_ty) = from_ty.kind() {
                        arg.as_ty(format!(
                            "u{}",
                            int_ty.bit_width().map_or_else(|| "size".to_string(), |v| v.to_string())
                        ))
                    } else {
                        arg
                    };
                    diag.span_suggestion(
                        e.span,
                        "consider using",
                        format!("{}::from_bits({})", to_ty, arg.to_string()),
                        Applicability::Unspecified,
                    );
                },
            );
            true
        },
        _ => false,
    }
}
