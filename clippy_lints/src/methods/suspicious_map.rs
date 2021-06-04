use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::usage::mutated_variables;
use clippy_utils::{expr_or_init, is_trait_method};
use if_chain::if_chain;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for calls to `map` followed by a `count`.
    ///
    /// **Why is this bad?** It looks suspicious. Maybe `map` was confused with `filter`.
    /// If the `map` call is intentional, this should be rewritten. Or, if you intend to
    /// drive the iterator to completion, you can just use `for_each` instead.
    ///
    /// **Known problems:** None
    ///
    /// **Example:**
    ///
    /// ```rust
    /// let _ = (0..3).map(|x| x + 2).count();
    /// ```
    pub SUSPICIOUS_MAP,
    complexity,
    "suspicious usage of map"
}

pub fn check<'tcx>(cx: &LateContext<'tcx>, expr: &hir::Expr<'_>, count_recv: &hir::Expr<'_>, map_arg: &hir::Expr<'_>) {
    if_chain! {
        if is_trait_method(cx, count_recv, sym::Iterator);
        let closure = expr_or_init(cx, map_arg);
        if let Some(body_id) = cx.tcx.hir().maybe_body_owned_by(closure.hir_id);
        let closure_body = cx.tcx.hir().body(body_id);
        if !cx.typeck_results().expr_ty(&closure_body.value).is_unit();
        then {
            if let Some(map_mutated_vars) = mutated_variables(&closure_body.value, cx) {
                // A variable is used mutably inside of the closure. Suppress the lint.
                if !map_mutated_vars.is_empty() {
                    return;
                }
            }
            span_lint_and_help(
                cx,
                SUSPICIOUS_MAP,
                expr.span,
                "this call to `map()` won't have an effect on the call to `count()`",
                None,
                "make sure you did not confuse `map` with `filter` or `for_each`",
            );
        }
    }
}
