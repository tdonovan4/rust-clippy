use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::is_trait_method;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for usage of `_.skip_while(condition).next()`.
    ///
    /// **Why is this bad?** Readability, this can be written more concisely as
    /// `_.find(!condition)`.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// # let vec = vec![1];
    /// vec.iter().skip_while(|x| **x == 0).next();
    /// ```
    /// Could be written as
    /// ```rust
    /// # let vec = vec![1];
    /// vec.iter().find(|x| **x != 0);
    /// ```
    pub SKIP_WHILE_NEXT,
    complexity,
    "using `skip_while(p).next()`, which is more succinctly expressed as `.find(!p)`"
}

/// lint use of `skip_while().next()` for `Iterators`
pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, expr: &'tcx hir::Expr<'_>) {
    // lint if caller of `.skip_while().next()` is an Iterator
    if is_trait_method(cx, expr, sym::Iterator) {
        span_lint_and_help(
            cx,
            SKIP_WHILE_NEXT,
            expr.span,
            "called `skip_while(<p>).next()` on an `Iterator`",
            None,
            "this is more succinctly expressed by calling `.find(!<p>)` instead",
        );
    }
}
