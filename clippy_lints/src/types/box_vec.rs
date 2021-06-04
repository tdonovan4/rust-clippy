use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::is_ty_param_diagnostic_item;
use rustc_hir::{self as hir, def_id::DefId, QPath};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for use of `Box<Vec<_>>` anywhere in the code.
    /// Check the [Box documentation](https://doc.rust-lang.org/std/boxed/index.html) for more information.
    ///
    /// **Why is this bad?** `Vec` already keeps its contents in a separate area on
    /// the heap. So if you `Box` it, you just add another level of indirection
    /// without any benefit whatsoever.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust,ignore
    /// struct X {
    ///     values: Box<Vec<Foo>>,
    /// }
    /// ```
    ///
    /// Better:
    ///
    /// ```rust,ignore
    /// struct X {
    ///     values: Vec<Foo>,
    /// }
    /// ```
    pub BOX_VEC,
    perf,
    "usage of `Box<Vec<T>>`, vector elements are already on the heap"
}

pub(super) fn check(cx: &LateContext<'_>, hir_ty: &hir::Ty<'_>, qpath: &QPath<'_>, def_id: DefId) -> bool {
    if Some(def_id) == cx.tcx.lang_items().owned_box()
        && is_ty_param_diagnostic_item(cx, qpath, sym::vec_type).is_some()
    {
        span_lint_and_help(
            cx,
            BOX_VEC,
            hir_ty.span,
            "you seem to be trying to use `Box<Vec<T>>`. Consider using just `Vec<T>`",
            None,
            "`Vec<T>` is already on the heap, `Box<Vec<T>>` makes an extra allocation",
        );
        true
    } else {
        false
    }
}
