use clippy_utils::diagnostics::span_lint_and_sugg;
use if_chain::if_chain;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_hir::ExprKind;
use rustc_lint::LateContext;
use rustc_middle::ty::TyS;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::Symbol;

use clippy_utils::is_diagnostic_assoc_item;

declare_clippy_lint! {
    /// **What it does:** Checks for the usage of `_.to_owned()`, `vec.to_vec()`, or similar when calling `_.clone()` would be clearer.
    ///
    /// **Why is this bad?** These methods do the same thing as `_.clone()` but may be confusing as
    /// to why we are calling `to_vec` on something that is already a `Vec` or calling `to_owned` on something that is already owned.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// let a = vec![1, 2, 3];
    /// let b = a.to_vec();
    /// let c = a.to_owned();
    /// ```
    /// Use instead:
    /// ```rust
    /// let a = vec![1, 2, 3];
    /// let b = a.clone();
    /// let c = a.clone();
    /// ```
    pub IMPLICIT_CLONE,
    pedantic,
    "implicitly cloning a value by invoking a function on its dereferenced type"
}

pub fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, trait_diagnostic: Symbol) {
    if_chain! {
        if let ExprKind::MethodCall(method_path, _, [arg], _) = &expr.kind;
        let return_type = cx.typeck_results().expr_ty(expr);
        let input_type = cx.typeck_results().expr_ty(arg).peel_refs();
        if let Some(expr_def_id) = cx.typeck_results().type_dependent_def_id(expr.hir_id);
        if let Some(ty_name) = input_type.ty_adt_def().map(|adt_def| cx.tcx.item_name(adt_def.did));
        if TyS::same_type(return_type, input_type);
        if is_diagnostic_assoc_item(cx, expr_def_id, trait_diagnostic);
        then {
            span_lint_and_sugg(
                cx,IMPLICIT_CLONE,method_path.ident.span,
                &format!("implicitly cloning a `{}` by calling `{}` on its dereferenced type", ty_name, method_path.ident.name),
                "consider using",
                "clone".to_string(),
                Applicability::MachineApplicable
            );
        }
    }
}
