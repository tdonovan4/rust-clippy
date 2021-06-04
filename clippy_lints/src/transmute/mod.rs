mod crosspointer_transmute;
mod transmute_float_to_int;
mod transmute_int_to_bool;
mod transmute_int_to_char;
mod transmute_int_to_float;
mod transmute_ptr_to_ptr;
mod transmute_ptr_to_ref;
mod transmute_ref_to_ref;
mod transmutes_expressible_as_ptr_casts;
mod unsound_collection_transmute;
mod useless_transmute;
mod utils;
mod wrong_transmute;

use clippy_utils::{in_constant, match_def_path, paths};
use if_chain::if_chain;
use rustc_hir::{Expr, ExprKind};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::declare_lint_pass;

pub use crosspointer_transmute::CROSSPOINTER_TRANSMUTE;
pub use transmute_float_to_int::TRANSMUTE_FLOAT_TO_INT;
pub use transmute_int_to_bool::TRANSMUTE_INT_TO_BOOL;
pub use transmute_int_to_char::TRANSMUTE_INT_TO_CHAR;
pub use transmute_int_to_float::TRANSMUTE_INT_TO_FLOAT;
pub use transmute_ptr_to_ptr::TRANSMUTE_PTR_TO_PTR;
pub use transmute_ptr_to_ref::TRANSMUTE_PTR_TO_REF;
pub use transmute_ref_to_ref::TRANSMUTE_BYTES_TO_STR;
pub use transmutes_expressible_as_ptr_casts::TRANSMUTES_EXPRESSIBLE_AS_PTR_CASTS;
pub use unsound_collection_transmute::UNSOUND_COLLECTION_TRANSMUTE;
pub use useless_transmute::USELESS_TRANSMUTE;
pub use wrong_transmute::WRONG_TRANSMUTE;

declare_lint_pass!(Transmute => [
    CROSSPOINTER_TRANSMUTE,
    TRANSMUTE_PTR_TO_REF,
    TRANSMUTE_PTR_TO_PTR,
    USELESS_TRANSMUTE,
    WRONG_TRANSMUTE,
    TRANSMUTE_INT_TO_CHAR,
    TRANSMUTE_BYTES_TO_STR,
    TRANSMUTE_INT_TO_BOOL,
    TRANSMUTE_INT_TO_FLOAT,
    TRANSMUTE_FLOAT_TO_INT,
    UNSOUND_COLLECTION_TRANSMUTE,
    TRANSMUTES_EXPRESSIBLE_AS_PTR_CASTS,
]);

impl<'tcx> LateLintPass<'tcx> for Transmute {
    #[allow(clippy::similar_names, clippy::too_many_lines)]
    fn check_expr(&mut self, cx: &LateContext<'tcx>, e: &'tcx Expr<'_>) {
        if_chain! {
            if let ExprKind::Call(path_expr, args) = e.kind;
            if let ExprKind::Path(ref qpath) = path_expr.kind;
            if let Some(def_id) = cx.qpath_res(qpath, path_expr.hir_id).opt_def_id();
            if match_def_path(cx, def_id, &paths::TRANSMUTE);
            then {
                // Avoid suggesting from/to bits and dereferencing raw pointers in const contexts.
                // See https://github.com/rust-lang/rust/issues/73736 for progress on making them `const fn`.
                // And see https://github.com/rust-lang/rust/issues/51911 for dereferencing raw pointers.
                let const_context = in_constant(cx, e.hir_id);

                let from_ty = cx.typeck_results().expr_ty(&args[0]);
                let to_ty = cx.typeck_results().expr_ty(e);

                // If useless_transmute is triggered, the other lints can be skipped.
                if useless_transmute::check(cx, e, from_ty, to_ty, args) {
                    return;
                }

                let mut linted = wrong_transmute::check(cx, e, from_ty, to_ty);
                linted |= crosspointer_transmute::check(cx, e, from_ty, to_ty);
                linted |= transmute_ptr_to_ref::check(cx, e, from_ty, to_ty, args, qpath);
                linted |= transmute_int_to_char::check(cx, e, from_ty, to_ty, args);
                linted |= transmute_ref_to_ref::check(cx, e, from_ty, to_ty, args, const_context);
                linted |= transmute_ptr_to_ptr::check(cx, e, from_ty, to_ty, args);
                linted |= transmute_int_to_bool::check(cx, e, from_ty, to_ty, args);
                linted |= transmute_int_to_float::check(cx, e, from_ty, to_ty, args, const_context);
                linted |= transmute_float_to_int::check(cx, e, from_ty, to_ty, args, const_context);
                linted |= unsound_collection_transmute::check(cx, e, from_ty, to_ty);

                if !linted {
                    transmutes_expressible_as_ptr_casts::check(cx, e, from_ty, to_ty, args);
                }
            }
        }
    }
}
