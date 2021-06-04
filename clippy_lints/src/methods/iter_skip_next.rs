use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::is_trait_method;
use clippy_utils::source::snippet;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for use of `.skip(x).next()` on iterators.
    ///
    /// **Why is this bad?** `.nth(x)` is cleaner
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let some_vec = vec![0, 1, 2, 3];
    /// let bad_vec = some_vec.iter().skip(3).next();
    /// let bad_slice = &some_vec[..].iter().skip(3).next();
    /// ```
    /// The correct use would be:
    /// ```rust
    /// let some_vec = vec![0, 1, 2, 3];
    /// let bad_vec = some_vec.iter().nth(3);
    /// let bad_slice = &some_vec[..].iter().nth(3);
    /// ```
    pub ITER_SKIP_NEXT,
    style,
    "using `.skip(x).next()` on an iterator"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, recv: &hir::Expr<'_>, arg: &hir::Expr<'_>) {
    // lint if caller of skip is an Iterator
    if is_trait_method(cx, expr, sym::Iterator) {
        span_lint_and_sugg(
            cx,
            ITER_SKIP_NEXT,
            expr.span.trim_start(recv.span).unwrap(),
            "called `skip(..).next()` on an iterator",
            "use `nth` instead",
            format!(".nth({})", snippet(cx, arg.span, "..")),
            Applicability::MachineApplicable,
        );
    }
}
