use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::higher;
use clippy_utils::source::snippet_with_macro_callsite;
use rustc_errors::Applicability;
use rustc_hir::{Stmt, StmtKind};
use rustc_lint::{LateContext, LintContext};
use rustc_middle::lint::in_external_macro;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for binding a unit value.
    ///
    /// **Why is this bad?** A unit value cannot usefully be used anywhere. So
    /// binding one is kind of pointless.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let x = {
    ///     1;
    /// };
    /// ```
    pub LET_UNIT_VALUE,
    pedantic,
    "creating a `let` binding to a value of unit type, which usually can't be used afterwards"
}

pub(super) fn check(cx: &LateContext<'_>, stmt: &Stmt<'_>) {
    if let StmtKind::Local(local) = stmt.kind {
        if cx.typeck_results().pat_ty(local.pat).is_unit() {
            if in_external_macro(cx.sess(), stmt.span) || local.pat.span.from_expansion() {
                return;
            }
            if higher::is_from_for_desugar(local) {
                return;
            }
            span_lint_and_then(
                cx,
                LET_UNIT_VALUE,
                stmt.span,
                "this let-binding has unit value",
                |diag| {
                    if let Some(expr) = &local.init {
                        let snip = snippet_with_macro_callsite(cx, expr.span, "()");
                        diag.span_suggestion(
                            stmt.span,
                            "omit the `let` binding",
                            format!("{};", snip),
                            Applicability::MachineApplicable, // snippet
                        );
                    }
                },
            );
        }
    }
}
