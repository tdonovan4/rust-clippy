use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::source::snippet_with_applicability;
use if_chain::if_chain;
use rustc_ast::LitKind;
use rustc_errors::Applicability;
use rustc_hir::{Expr, ExprKind};
use rustc_lint::LateContext;
use rustc_middle::ty::{self, UintTy};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for expressions where a character literal is cast
    /// to `u8` and suggests using a byte literal instead.
    ///
    /// **Why is this bad?** In general, casting values to smaller types is
    /// error-prone and should be avoided where possible. In the particular case of
    /// converting a character literal to u8, it is easy to avoid by just using a
    /// byte literal instead. As an added bonus, `b'a'` is even slightly shorter
    /// than `'a' as u8`.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust,ignore
    /// 'x' as u8
    /// ```
    ///
    /// A better version, using the byte literal:
    ///
    /// ```rust,ignore
    /// b'x'
    /// ```
    pub CHAR_LIT_AS_U8,
    complexity,
    "casting a character literal to `u8` truncates"
}

pub(super) fn check(cx: &LateContext<'tcx>, expr: &'tcx Expr<'_>) {
    if_chain! {
        if let ExprKind::Cast(e, _) = &expr.kind;
        if let ExprKind::Lit(l) = &e.kind;
        if let LitKind::Char(c) = l.node;
        if ty::Uint(UintTy::U8) == *cx.typeck_results().expr_ty(expr).kind();
        then {
            let mut applicability = Applicability::MachineApplicable;
            let snippet = snippet_with_applicability(cx, e.span, "'x'", &mut applicability);

            span_lint_and_then(
                cx,
                CHAR_LIT_AS_U8,
                expr.span,
                "casting a character literal to `u8` truncates",
                |diag| {
                    diag.note("`char` is four bytes wide, but `u8` is a single byte");

                    if c.is_ascii() {
                        diag.span_suggestion(
                            expr.span,
                            "use a byte literal instead",
                            format!("b{}", snippet),
                            applicability,
                        );
                    }
            });
        }
    }
}
