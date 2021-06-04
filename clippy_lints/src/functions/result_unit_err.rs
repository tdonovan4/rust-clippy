use rustc_hir as hir;
use rustc_lint::{LateContext, LintContext};
use rustc_middle::lint::in_external_macro;
use rustc_middle::ty;
use rustc_session::declare_tool_lint;
use rustc_span::{sym, Span};
use rustc_typeck::hir_ty_to_ty;

use if_chain::if_chain;

use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::trait_ref_of_method;
use clippy_utils::ty::is_type_diagnostic_item;

declare_clippy_lint! {
    /// **What it does:** Checks for public functions that return a `Result`
    /// with an `Err` type of `()`. It suggests using a custom type that
    /// implements `std::error::Error`.
    ///
    /// **Why is this bad?** Unit does not implement `Error` and carries no
    /// further information about what went wrong.
    ///
    /// **Known problems:** Of course, this lint assumes that `Result` is used
    /// for a fallible operation (which is after all the intended use). However
    /// code may opt to (mis)use it as a basic two-variant-enum. In that case,
    /// the suggestion is misguided, and the code should use a custom enum
    /// instead.
    ///
    /// **Examples:**
    /// ```rust
    /// pub fn read_u8() -> Result<u8, ()> { Err(()) }
    /// ```
    /// should become
    /// ```rust,should_panic
    /// use std::fmt;
    ///
    /// #[derive(Debug)]
    /// pub struct EndOfStream;
    ///
    /// impl fmt::Display for EndOfStream {
    ///     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    ///         write!(f, "End of Stream")
    ///     }
    /// }
    ///
    /// impl std::error::Error for EndOfStream { }
    ///
    /// pub fn read_u8() -> Result<u8, EndOfStream> { Err(EndOfStream) }
    ///# fn main() {
    ///#     read_u8().unwrap();
    ///# }
    /// ```
    ///
    /// Note that there are crates that simplify creating the error type, e.g.
    /// [`thiserror`](https://docs.rs/thiserror).
    pub RESULT_UNIT_ERR,
    style,
    "public function returning `Result` with an `Err` type of `()`"
}

pub(super) fn check_item(cx: &LateContext<'tcx>, item: &'tcx hir::Item<'_>) {
    if let hir::ItemKind::Fn(ref sig, ref _generics, _) = item.kind {
        let is_public = cx.access_levels.is_exported(item.hir_id());
        let fn_header_span = item.span.with_hi(sig.decl.output.span().hi());
        if is_public {
            check_result_unit_err(cx, sig.decl, item.span, fn_header_span);
        }
    }
}

pub(super) fn check_impl_item(cx: &LateContext<'tcx>, item: &'tcx hir::ImplItem<'_>) {
    if let hir::ImplItemKind::Fn(ref sig, _) = item.kind {
        let is_public = cx.access_levels.is_exported(item.hir_id());
        let fn_header_span = item.span.with_hi(sig.decl.output.span().hi());
        if is_public && trait_ref_of_method(cx, item.hir_id()).is_none() {
            check_result_unit_err(cx, sig.decl, item.span, fn_header_span);
        }
    }
}

pub(super) fn check_trait_item(cx: &LateContext<'tcx>, item: &'tcx hir::TraitItem<'_>) {
    if let hir::TraitItemKind::Fn(ref sig, _) = item.kind {
        let is_public = cx.access_levels.is_exported(item.hir_id());
        let fn_header_span = item.span.with_hi(sig.decl.output.span().hi());
        if is_public {
            check_result_unit_err(cx, sig.decl, item.span, fn_header_span);
        }
    }
}

fn check_result_unit_err(cx: &LateContext<'_>, decl: &hir::FnDecl<'_>, item_span: Span, fn_header_span: Span) {
    if_chain! {
        if !in_external_macro(cx.sess(), item_span);
        if let hir::FnRetTy::Return(ty) = decl.output;
        let ty = hir_ty_to_ty(cx.tcx, ty);
        if is_type_diagnostic_item(cx, ty, sym::result_type);
        if let ty::Adt(_, substs) = ty.kind();
        let err_ty = substs.type_at(1);
        if err_ty.is_unit();
        then {
            span_lint_and_help(
                cx,
                RESULT_UNIT_ERR,
                fn_header_span,
                "this returns a `Result<_, ()>`",
                None,
                "use a custom `Error` type instead",
            );
        }
    }
}
