mod cast_lossless;
mod cast_possible_truncation;
mod cast_possible_wrap;
mod cast_precision_loss;
mod cast_ptr_alignment;
mod cast_ref_to_mut;
mod cast_sign_loss;
mod char_lit_as_u8;
mod fn_to_numeric_cast;
mod fn_to_numeric_cast_with_truncation;
mod ptr_as_ptr;
mod unnecessary_cast;
mod utils;

use clippy_utils::is_hir_ty_cfg_dependant;
use rustc_hir::{Expr, ExprKind};
use rustc_lint::{LateContext, LateLintPass, LintContext};
use rustc_middle::lint::in_external_macro;
use rustc_semver::RustcVersion;
use rustc_session::impl_lint_pass;

pub use cast_lossless::CAST_LOSSLESS;
pub use cast_possible_truncation::CAST_POSSIBLE_TRUNCATION;
pub use cast_possible_wrap::CAST_POSSIBLE_WRAP;
pub use cast_precision_loss::CAST_PRECISION_LOSS;
pub use cast_ptr_alignment::CAST_PTR_ALIGNMENT;
pub use cast_ref_to_mut::CAST_REF_TO_MUT;
pub use cast_sign_loss::CAST_SIGN_LOSS;
pub use char_lit_as_u8::CHAR_LIT_AS_U8;
pub use fn_to_numeric_cast::FN_TO_NUMERIC_CAST;
pub use fn_to_numeric_cast_with_truncation::FN_TO_NUMERIC_CAST_WITH_TRUNCATION;
pub use ptr_as_ptr::PTR_AS_PTR;
pub use unnecessary_cast::UNNECESSARY_CAST;

pub struct Casts {
    msrv: Option<RustcVersion>,
}

impl Casts {
    #[must_use]
    pub fn new(msrv: Option<RustcVersion>) -> Self {
        Self { msrv }
    }
}

impl_lint_pass!(Casts => [
    CAST_PRECISION_LOSS,
    CAST_SIGN_LOSS,
    CAST_POSSIBLE_TRUNCATION,
    CAST_POSSIBLE_WRAP,
    CAST_LOSSLESS,
    CAST_REF_TO_MUT,
    CAST_PTR_ALIGNMENT,
    UNNECESSARY_CAST,
    FN_TO_NUMERIC_CAST,
    FN_TO_NUMERIC_CAST_WITH_TRUNCATION,
    CHAR_LIT_AS_U8,
    PTR_AS_PTR,
]);

impl<'tcx> LateLintPass<'tcx> for Casts {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx Expr<'_>) {
        if expr.span.from_expansion() {
            return;
        }

        if let ExprKind::Cast(cast_expr, cast_to) = expr.kind {
            if is_hir_ty_cfg_dependant(cx, cast_to) {
                return;
            }
            let (cast_from, cast_to) = (
                cx.typeck_results().expr_ty(cast_expr),
                cx.typeck_results().expr_ty(expr),
            );

            if unnecessary_cast::check(cx, expr, cast_expr, cast_from, cast_to) {
                return;
            }

            fn_to_numeric_cast::check(cx, expr, cast_expr, cast_from, cast_to);
            fn_to_numeric_cast_with_truncation::check(cx, expr, cast_expr, cast_from, cast_to);
            if cast_from.is_numeric() && cast_to.is_numeric() && !in_external_macro(cx.sess(), expr.span) {
                cast_possible_truncation::check(cx, expr, cast_from, cast_to);
                cast_possible_wrap::check(cx, expr, cast_from, cast_to);
                cast_precision_loss::check(cx, expr, cast_from, cast_to);
                cast_lossless::check(cx, expr, cast_expr, cast_from, cast_to);
                cast_sign_loss::check(cx, expr, cast_expr, cast_from, cast_to);
            }
        }

        cast_ref_to_mut::check(cx, expr);
        cast_ptr_alignment::check(cx, expr);
        char_lit_as_u8::check(cx, expr);
        ptr_as_ptr::check(cx, expr, &self.msrv);
    }

    extract_msrv_attr!(LateContext);
}
