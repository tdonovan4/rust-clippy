use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::{is_trait_method, match_qpath, path_to_local_id, paths};
use if_chain::if_chain;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::{source_map::Span, sym};

declare_clippy_lint! {
    /// **What it does:** Checks for usage of `filter_map(|x| x)`.
    ///
    /// **Why is this bad?** Readability, this can be written more concisely by using `flatten`.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// # let iter = vec![Some(1)].into_iter();
    /// iter.filter_map(|x| x);
    /// ```
    /// Use instead:
    /// ```rust
    /// # let iter = vec![Some(1)].into_iter();
    /// iter.flatten();
    /// ```
    pub FILTER_MAP_IDENTITY,
    complexity,
    "call to `filter_map` where `flatten` is sufficient"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, filter_map_arg: &hir::Expr<'_>, filter_map_span: Span) {
    if is_trait_method(cx, expr, sym::Iterator) {
        let apply_lint = |message: &str| {
            span_lint_and_sugg(
                cx,
                FILTER_MAP_IDENTITY,
                filter_map_span.with_hi(expr.span.hi()),
                message,
                "try",
                "flatten()".to_string(),
                Applicability::MachineApplicable,
            );
        };

        if_chain! {
            if let hir::ExprKind::Closure(_, _, body_id, _, _) = filter_map_arg.kind;
            let body = cx.tcx.hir().body(body_id);

            if let hir::PatKind::Binding(_, binding_id, ..) = body.params[0].pat.kind;
            if path_to_local_id(&body.value, binding_id);
            then {
                apply_lint("called `filter_map(|x| x)` on an `Iterator`");
            }
        }

        if_chain! {
            if let hir::ExprKind::Path(ref qpath) = filter_map_arg.kind;

            if match_qpath(qpath, &paths::STD_CONVERT_IDENTITY);

            then {
                apply_lint("called `filter_map(std::convert::identity)` on an `Iterator`");
            }
        }
    }
}
