use clippy_utils::diagnostics::span_lint;
use rustc_hir::{BinOpKind, Expr, ExprKind};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::hygiene::{ExpnKind, MacroKind};

declare_clippy_lint! {
    /// **What it does:** Checks for comparisons to unit. This includes all binary
    /// comparisons (like `==` and `<`) and asserts.
    ///
    /// **Why is this bad?** Unit is always equal to itself, and thus is just a
    /// clumsily written constant. Mostly this happens when someone accidentally
    /// adds semicolons at the end of the operands.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// # fn foo() {};
    /// # fn bar() {};
    /// # fn baz() {};
    /// if {
    ///     foo();
    /// } == {
    ///     bar();
    /// } {
    ///     baz();
    /// }
    /// ```
    /// is equal to
    /// ```rust
    /// # fn foo() {};
    /// # fn bar() {};
    /// # fn baz() {};
    /// {
    ///     foo();
    ///     bar();
    ///     baz();
    /// }
    /// ```
    ///
    /// For asserts:
    /// ```rust
    /// # fn foo() {};
    /// # fn bar() {};
    /// assert_eq!({ foo(); }, { bar(); });
    /// ```
    /// will always succeed
    pub UNIT_CMP,
    correctness,
    "comparing unit values"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &Expr<'_>) {
    if expr.span.from_expansion() {
        if let Some(callee) = expr.span.source_callee() {
            if let ExpnKind::Macro(MacroKind::Bang, symbol) = callee.kind {
                if let ExprKind::Binary(ref cmp, left, _) = expr.kind {
                    let op = cmp.node;
                    if op.is_comparison() && cx.typeck_results().expr_ty(left).is_unit() {
                        let result = match &*symbol.as_str() {
                            "assert_eq" | "debug_assert_eq" => "succeed",
                            "assert_ne" | "debug_assert_ne" => "fail",
                            _ => return,
                        };
                        span_lint(
                            cx,
                            UNIT_CMP,
                            expr.span,
                            &format!(
                                "`{}` of unit values detected. This will always {}",
                                symbol.as_str(),
                                result
                            ),
                        );
                    }
                }
            }
        }
        return;
    }

    if let ExprKind::Binary(ref cmp, left, _) = expr.kind {
        let op = cmp.node;
        if op.is_comparison() && cx.typeck_results().expr_ty(left).is_unit() {
            let result = match op {
                BinOpKind::Eq | BinOpKind::Le | BinOpKind::Ge => "true",
                _ => "false",
            };
            span_lint(
                cx,
                UNIT_CMP,
                expr.span,
                &format!(
                    "{}-comparison of unit values detected. This will always be {}",
                    op.as_str(),
                    result
                ),
            );
        }
    }
}
