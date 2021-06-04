use clippy_utils::diagnostics::span_lint;
use clippy_utils::is_ty_param_diagnostic_item;
use rustc_hir::{self as hir, def_id::DefId, QPath};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for use of `Option<Option<_>>` in function signatures and type
    /// definitions
    ///
    /// **Why is this bad?** `Option<_>` represents an optional value. `Option<Option<_>>`
    /// represents an optional optional value which is logically the same thing as an optional
    /// value but has an unneeded extra level of wrapping.
    ///
    /// If you have a case where `Some(Some(_))`, `Some(None)` and `None` are distinct cases,
    /// consider a custom `enum` instead, with clear names for each case.
    ///
    /// **Known problems:** None.
    ///
    /// **Example**
    /// ```rust
    /// fn get_data() -> Option<Option<u32>> {
    ///     None
    /// }
    /// ```
    ///
    /// Better:
    ///
    /// ```rust
    /// pub enum Contents {
    ///     Data(Vec<u8>), // Was Some(Some(Vec<u8>))
    ///     NotYetFetched, // Was Some(None)
    ///     None,          // Was None
    /// }
    ///
    /// fn get_data() -> Contents {
    ///     Contents::None
    /// }
    /// ```
    pub OPTION_OPTION,
    pedantic,
    "usage of `Option<Option<T>>`"
}

pub(super) fn check(cx: &LateContext<'_>, hir_ty: &hir::Ty<'_>, qpath: &QPath<'_>, def_id: DefId) -> bool {
    if cx.tcx.is_diagnostic_item(sym::option_type, def_id)
        && is_ty_param_diagnostic_item(cx, qpath, sym::option_type).is_some()
    {
        span_lint(
            cx,
            OPTION_OPTION,
            hir_ty.span,
            "consider using `Option<T>` instead of `Option<Option<T>>` or a custom \
                                 enum if you need to distinguish all 3 cases",
        );
        true
    } else {
        false
    }
}
