mod must_use;
mod not_unsafe_ptr_arg_deref;
mod result_unit_err;
mod too_many_arguments;
mod too_many_lines;

use rustc_hir as hir;
use rustc_hir::intravisit;
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::impl_lint_pass;
use rustc_span::Span;

pub use must_use::{DOUBLE_MUST_USE, MUST_USE_CANDIDATE, MUST_USE_UNIT};
pub use not_unsafe_ptr_arg_deref::NOT_UNSAFE_PTR_ARG_DEREF;
pub use result_unit_err::RESULT_UNIT_ERR;
pub use too_many_arguments::TOO_MANY_ARGUMENTS;
pub use too_many_lines::TOO_MANY_LINES;

#[derive(Copy, Clone)]
pub struct Functions {
    too_many_arguments_threshold: u64,
    too_many_lines_threshold: u64,
}

impl Functions {
    pub fn new(too_many_arguments_threshold: u64, too_many_lines_threshold: u64) -> Self {
        Self {
            too_many_arguments_threshold,
            too_many_lines_threshold,
        }
    }
}

impl_lint_pass!(Functions => [
    TOO_MANY_ARGUMENTS,
    TOO_MANY_LINES,
    NOT_UNSAFE_PTR_ARG_DEREF,
    MUST_USE_UNIT,
    DOUBLE_MUST_USE,
    MUST_USE_CANDIDATE,
    RESULT_UNIT_ERR,
]);

impl<'tcx> LateLintPass<'tcx> for Functions {
    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        kind: intravisit::FnKind<'tcx>,
        decl: &'tcx hir::FnDecl<'_>,
        body: &'tcx hir::Body<'_>,
        span: Span,
        hir_id: hir::HirId,
    ) {
        too_many_arguments::check_fn(cx, kind, decl, span, hir_id, self.too_many_arguments_threshold);
        too_many_lines::check_fn(cx, span, body, self.too_many_lines_threshold);
        not_unsafe_ptr_arg_deref::check_fn(cx, kind, decl, body, hir_id);
    }

    fn check_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx hir::Item<'_>) {
        must_use::check_item(cx, item);
        result_unit_err::check_item(cx, item);
    }

    fn check_impl_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx hir::ImplItem<'_>) {
        must_use::check_impl_item(cx, item);
        result_unit_err::check_impl_item(cx, item);
    }

    fn check_trait_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx hir::TraitItem<'_>) {
        too_many_arguments::check_trait_item(cx, item, self.too_many_arguments_threshold);
        not_unsafe_ptr_arg_deref::check_trait_item(cx, item);
        must_use::check_trait_item(cx, item);
        result_unit_err::check_trait_item(cx, item);
    }
}
