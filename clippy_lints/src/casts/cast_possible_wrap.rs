use clippy_utils::diagnostics::span_lint;
use clippy_utils::ty::is_isize_or_usize;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::Ty;
use rustc_session::declare_tool_lint;

use super::utils;

declare_clippy_lint! {
    /// **What it does:** Checks for casts from an unsigned type to a signed type of
    /// the same size. Performing such a cast is a 'no-op' for the compiler,
    /// i.e., nothing is changed at the bit level, and the binary representation of
    /// the value is reinterpreted. This can cause wrapping if the value is too big
    /// for the target signed type. However, the cast works as defined, so this lint
    /// is `Allow` by default.
    ///
    /// **Why is this bad?** While such a cast is not bad in itself, the results can
    /// be surprising when this is not the intended behavior, as demonstrated by the
    /// example below.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// u32::MAX as i32; // will yield a value of `-1`
    /// ```
    pub CAST_POSSIBLE_WRAP,
    pedantic,
    "casts that may cause wrapping around the value, e.g., `x as i32` where `x: u32` and `x > i32::MAX`"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &Expr<'_>, cast_from: Ty<'_>, cast_to: Ty<'_>) {
    if !(cast_from.is_integral() && cast_to.is_integral()) {
        return;
    }

    let arch_64_suffix = " on targets with 64-bit wide pointers";
    let arch_32_suffix = " on targets with 32-bit wide pointers";
    let cast_unsigned_to_signed = !cast_from.is_signed() && cast_to.is_signed();
    let from_nbits = utils::int_ty_to_nbits(cast_from, cx.tcx);
    let to_nbits = utils::int_ty_to_nbits(cast_to, cx.tcx);

    let (should_lint, suffix) = match (is_isize_or_usize(cast_from), is_isize_or_usize(cast_to)) {
        (true, true) | (false, false) => (to_nbits == from_nbits && cast_unsigned_to_signed, ""),
        (true, false) => (to_nbits <= 32 && cast_unsigned_to_signed, arch_32_suffix),
        (false, true) => (
            cast_unsigned_to_signed,
            if from_nbits == 64 {
                arch_64_suffix
            } else {
                arch_32_suffix
            },
        ),
    };

    if should_lint {
        span_lint(
            cx,
            CAST_POSSIBLE_WRAP,
            expr.span,
            &format!(
                "casting `{}` to `{}` may wrap around the value{}",
                cast_from, cast_to, suffix,
            ),
        );
    }
}
