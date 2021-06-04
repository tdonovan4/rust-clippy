use super::utils::get_hint_if_single_char_arg;
use clippy_utils::diagnostics::span_lint_and_sugg;
use if_chain::if_chain;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_middle::ty;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::Symbol;

declare_clippy_lint! {
    /// **What it does:** Checks for string methods that receive a single-character
    /// `str` as an argument, e.g., `_.split("x")`.
    ///
    /// **Why is this bad?** Performing these methods using a `char` is faster than
    /// using a `str`.
    ///
    /// **Known problems:** Does not catch multi-byte unicode characters.
    ///
    /// **Example:**
    /// ```rust,ignore
    /// // Bad
    /// _.split("x");
    ///
    /// // Good
    /// _.split('x');
    pub SINGLE_CHAR_PATTERN,
    perf,
    "using a single-character str where a char could be used, e.g., `_.split(\"x\")`"
}

/// lint for length-1 `str`s for methods in `PATTERN_METHODS`
pub(super) fn check(cx: &LateContext<'_>, _expr: &hir::Expr<'_>, method_name: Symbol, args: &[hir::Expr<'_>]) {
    for &(method, pos) in &super::utils::PATTERN_METHODS {
        if_chain! {
            if let ty::Ref(_, ty, _) = cx.typeck_results().expr_ty_adjusted(&args[0]).kind();
            if *ty.kind() == ty::Str;
            if method_name.as_str() == method && args.len() > pos;
            let arg = &args[pos];
            let mut applicability = Applicability::MachineApplicable;
            if let Some(hint) = get_hint_if_single_char_arg(cx, arg, &mut applicability);
            then {
                span_lint_and_sugg(
                    cx,
                    SINGLE_CHAR_PATTERN,
                    arg.span,
                    "single-character string constant used as pattern",
                    "try using a `char` instead",
                    hint,
                    applicability,
                );
            }
        }
    }
}
