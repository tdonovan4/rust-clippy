use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet_with_applicability;
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty, UintTy};
use rustc_session::declare_tool_lint;

use super::utils;

declare_clippy_lint! {
    /// **What it does:** Checks for casts of function pointers to something other than usize
    ///
    /// **Why is this bad?**
    /// Casting a function pointer to anything other than usize/isize is not portable across
    /// architectures, because you end up losing bits if the target type is too small or end up with a
    /// bunch of extra bits that waste space and add more instructions to the final binary than
    /// strictly necessary for the problem
    ///
    /// Casting to isize also doesn't make sense since there are no signed addresses.
    ///
    /// **Example**
    ///
    /// ```rust
    /// // Bad
    /// fn fun() -> i32 { 1 }
    /// let a = fun as i64;
    ///
    /// // Good
    /// fn fun2() -> i32 { 1 }
    /// let a = fun2 as usize;
    /// ```
    pub FN_TO_NUMERIC_CAST,
    style,
    "casting a function pointer to a numeric type other than usize"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &Expr<'_>, cast_expr: &Expr<'_>, cast_from: Ty<'_>, cast_to: Ty<'_>) {
    // We only want to check casts to `ty::Uint` or `ty::Int`
    match cast_to.kind() {
        ty::Uint(_) | ty::Int(..) => { /* continue on */ },
        _ => return,
    }

    match cast_from.kind() {
        ty::FnDef(..) | ty::FnPtr(_) => {
            let mut applicability = Applicability::MaybeIncorrect;
            let from_snippet = snippet_with_applicability(cx, cast_expr.span, "x", &mut applicability);
            let to_nbits = utils::int_ty_to_nbits(cast_to, cx.tcx);

            if (to_nbits >= cx.tcx.data_layout.pointer_size.bits()) && (*cast_to.kind() != ty::Uint(UintTy::Usize)) {
                span_lint_and_sugg(
                    cx,
                    FN_TO_NUMERIC_CAST,
                    expr.span,
                    &format!("casting function pointer `{}` to `{}`", from_snippet, cast_to),
                    "try",
                    format!("{} as usize", from_snippet),
                    applicability,
                );
            }
        },
        _ => {},
    }
}
