use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::sugg;
use rustc_ast as ast;
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for transmutes from an integer to a `char`.
    ///
    /// **Why is this bad?** Not every integer is a Unicode scalar value.
    ///
    /// **Known problems:**
    /// - [`from_u32`] which this lint suggests using is slower than `transmute`
    /// as it needs to validate the input.
    /// If you are certain that the input is always a valid Unicode scalar value,
    /// use [`from_u32_unchecked`] which is as fast as `transmute`
    /// but has a semantically meaningful name.
    /// - You might want to handle `None` returned from [`from_u32`] instead of calling `unwrap`.
    ///
    /// [`from_u32`]: https://doc.rust-lang.org/std/char/fn.from_u32.html
    /// [`from_u32_unchecked`]: https://doc.rust-lang.org/std/char/fn.from_u32_unchecked.html
    ///
    /// **Example:**
    /// ```rust
    /// let x = 1_u32;
    /// unsafe {
    ///     let _: char = std::mem::transmute(x); // where x: u32
    /// }
    ///
    /// // should be:
    /// let _ = std::char::from_u32(x).unwrap();
    /// ```
    pub TRANSMUTE_INT_TO_CHAR,
    complexity,
    "transmutes from an integer to a `char`"
}

/// Checks for `transmute_int_to_char` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    e: &'tcx Expr<'_>,
    from_ty: Ty<'tcx>,
    to_ty: Ty<'tcx>,
    args: &'tcx [Expr<'_>],
) -> bool {
    match (&from_ty.kind(), &to_ty.kind()) {
        (ty::Int(ty::IntTy::I32) | ty::Uint(ty::UintTy::U32), &ty::Char) => {
            span_lint_and_then(
                cx,
                TRANSMUTE_INT_TO_CHAR,
                e.span,
                &format!("transmute from a `{}` to a `char`", from_ty),
                |diag| {
                    let arg = sugg::Sugg::hir(cx, &args[0], "..");
                    let arg = if let ty::Int(_) = from_ty.kind() {
                        arg.as_ty(ast::UintTy::U32.name_str())
                    } else {
                        arg
                    };
                    diag.span_suggestion(
                        e.span,
                        "consider using",
                        format!("std::char::from_u32({}).unwrap()", arg.to_string()),
                        Applicability::Unspecified,
                    );
                },
            );
            true
        },
        _ => false,
    }
}
