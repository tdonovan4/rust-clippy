use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet;
use clippy_utils::ty::is_type_diagnostic_item;
use clippy_utils::{eager_or_lazy, usage};
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** As the counterpart to `or_fun_call`, this lint looks for unnecessary
    /// lazily evaluated closures on `Option` and `Result`.
    ///
    /// This lint suggests changing the following functions, when eager evaluation results in
    /// simpler code:
    ///  - `unwrap_or_else` to `unwrap_or`
    ///  - `and_then` to `and`
    ///  - `or_else` to `or`
    ///  - `get_or_insert_with` to `get_or_insert`
    ///  - `ok_or_else` to `ok_or`
    ///
    /// **Why is this bad?** Using eager evaluation is shorter and simpler in some cases.
    ///
    /// **Known problems:** It is possible, but not recommended for `Deref` and `Index` to have
    /// side effects. Eagerly evaluating them can change the semantics of the program.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// // example code where clippy issues a warning
    /// let opt: Option<u32> = None;
    ///
    /// opt.unwrap_or_else(|| 42);
    /// ```
    /// Use instead:
    /// ```rust
    /// let opt: Option<u32> = None;
    ///
    /// opt.unwrap_or(42);
    /// ```
    pub UNNECESSARY_LAZY_EVALUATIONS,
    style,
    "using unnecessary lazy evaluation, which can be replaced with simpler eager evaluation"
}

/// lint use of `<fn>_else(simple closure)` for `Option`s and `Result`s that can be
/// replaced with `<fn>(return value of simple closure)`
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &'tcx hir::Expr<'_>,
    recv: &'tcx hir::Expr<'_>,
    arg: &'tcx hir::Expr<'_>,
    simplify_using: &str,
) {
    let is_option = is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(recv), sym::option_type);
    let is_result = is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(recv), sym::result_type);

    if is_option || is_result {
        if let hir::ExprKind::Closure(_, _, eid, _, _) = arg.kind {
            let body = cx.tcx.hir().body(eid);
            let body_expr = &body.value;

            if usage::BindingUsageFinder::are_params_used(cx, body) {
                return;
            }

            if eager_or_lazy::is_eagerness_candidate(cx, body_expr) {
                let msg = if is_option {
                    "unnecessary closure used to substitute value for `Option::None`"
                } else {
                    "unnecessary closure used to substitute value for `Result::Err`"
                };
                let applicability = if body
                    .params
                    .iter()
                    // bindings are checked to be unused above
                    .all(|param| matches!(param.pat.kind, hir::PatKind::Binding(..) | hir::PatKind::Wild))
                {
                    Applicability::MachineApplicable
                } else {
                    // replacing the lambda may break type inference
                    Applicability::MaybeIncorrect
                };

                span_lint_and_sugg(
                    cx,
                    UNNECESSARY_LAZY_EVALUATIONS,
                    expr.span,
                    msg,
                    &format!("use `{}` instead", simplify_using),
                    format!(
                        "{0}.{1}({2})",
                        snippet(cx, recv.span, ".."),
                        simplify_using,
                        snippet(cx, body_expr.span, ".."),
                    ),
                    applicability,
                );
            }
        }
    }
}
