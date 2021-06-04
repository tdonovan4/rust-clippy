use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::paths;
use clippy_utils::source::snippet_with_macro_callsite;
use clippy_utils::ty::{is_type_diagnostic_item, match_type};
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_middle::ty;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::{sym, Symbol};

declare_clippy_lint! {
    /// **What it does:** Checks for usage of `.clone()` on a ref-counted pointer,
    /// (`Rc`, `Arc`, `rc::Weak`, or `sync::Weak`), and suggests calling Clone via unified
    /// function syntax instead (e.g., `Rc::clone(foo)`).
    ///
    /// **Why is this bad?** Calling '.clone()' on an Rc, Arc, or Weak
    /// can obscure the fact that only the pointer is being cloned, not the underlying
    /// data.
    ///
    /// **Example:**
    /// ```rust
    /// # use std::rc::Rc;
    /// let x = Rc::new(1);
    ///
    /// // Bad
    /// x.clone();
    ///
    /// // Good
    /// Rc::clone(&x);
    /// ```
    pub CLONE_ON_REF_PTR,
    restriction,
    "using 'clone' on a ref-counted pointer"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, method_name: Symbol, args: &[hir::Expr<'_>]) {
    if !(args.len() == 1 && method_name == sym::clone) {
        return;
    }
    let arg = &args[0];
    let obj_ty = cx.typeck_results().expr_ty(arg).peel_refs();

    if let ty::Adt(_, subst) = obj_ty.kind() {
        let caller_type = if is_type_diagnostic_item(cx, obj_ty, sym::Rc) {
            "Rc"
        } else if is_type_diagnostic_item(cx, obj_ty, sym::Arc) {
            "Arc"
        } else if match_type(cx, obj_ty, &paths::WEAK_RC) || match_type(cx, obj_ty, &paths::WEAK_ARC) {
            "Weak"
        } else {
            return;
        };

        let snippet = snippet_with_macro_callsite(cx, arg.span, "..");

        span_lint_and_sugg(
            cx,
            CLONE_ON_REF_PTR,
            expr.span,
            "using `.clone()` on a ref-counted pointer",
            "try this",
            format!("{}::<{}>::clone(&{})", caller_type, subst.type_at(0), snippet),
            Applicability::Unspecified, // Sometimes unnecessary ::<_> after Rc/Arc/Weak
        );
    }
}
