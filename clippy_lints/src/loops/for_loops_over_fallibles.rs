use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::source::snippet;
use clippy_utils::ty::is_type_diagnostic_item;
use rustc_hir::{Expr, Pat};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for `for` loops over `Option` or `Result` values.
    ///
    /// **Why is this bad?** Readability. This is more clearly expressed as an `if
    /// let`.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// # let opt = Some(1);
    ///
    /// // Bad
    /// for x in opt {
    ///     // ..
    /// }
    ///
    /// // Good
    /// if let Some(x) = opt {
    ///     // ..
    /// }
    /// ```
    ///
    /// // or
    ///
    /// ```rust
    /// # let res: Result<i32, std::io::Error> = Ok(1);
    ///
    /// // Bad
    /// for x in &res {
    ///     // ..
    /// }
    ///
    /// // Good
    /// if let Ok(x) = res {
    ///     // ..
    /// }
    /// ```
    pub FOR_LOOPS_OVER_FALLIBLES,
    correctness,
    "for-looping over an `Option` or a `Result`, which is more clearly expressed as an `if let`"
}

/// Checks for `for` loops over `Option`s and `Result`s.
pub(super) fn check(cx: &LateContext<'_>, pat: &Pat<'_>, arg: &Expr<'_>) {
    let ty = cx.typeck_results().expr_ty(arg);
    if is_type_diagnostic_item(cx, ty, sym::option_type) {
        span_lint_and_help(
            cx,
            FOR_LOOPS_OVER_FALLIBLES,
            arg.span,
            &format!(
                "for loop over `{0}`, which is an `Option`. This is more readably written as an \
                `if let` statement",
                snippet(cx, arg.span, "_")
            ),
            None,
            &format!(
                "consider replacing `for {0} in {1}` with `if let Some({0}) = {1}`",
                snippet(cx, pat.span, "_"),
                snippet(cx, arg.span, "_")
            ),
        );
    } else if is_type_diagnostic_item(cx, ty, sym::result_type) {
        span_lint_and_help(
            cx,
            FOR_LOOPS_OVER_FALLIBLES,
            arg.span,
            &format!(
                "for loop over `{0}`, which is a `Result`. This is more readably written as an \
                `if let` statement",
                snippet(cx, arg.span, "_")
            ),
            None,
            &format!(
                "consider replacing `for {0} in {1}` with `if let Ok({0}) = {1}`",
                snippet(cx, pat.span, "_"),
                snippet(cx, arg.span, "_")
            ),
        );
    }
}
