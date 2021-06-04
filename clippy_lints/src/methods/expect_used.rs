use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::ty::is_type_diagnostic_item;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for `.expect()` calls on `Option`s and `Result`s.
    ///
    /// **Why is this bad?** Usually it is better to handle the `None` or `Err` case.
    /// Still, for a lot of quick-and-dirty code, `expect` is a good choice, which is why
    /// this lint is `Allow` by default.
    ///
    /// `result.expect()` will let the thread panic on `Err`
    /// values. Normally, you want to implement more sophisticated error handling,
    /// and propagate errors upwards with `?` operator.
    ///
    /// **Known problems:** None.
    ///
    /// **Examples:**
    /// ```rust,ignore
    /// # let opt = Some(1);
    ///
    /// // Bad
    /// opt.expect("one");
    ///
    /// // Good
    /// let opt = Some(1);
    /// opt?;
    /// ```
    ///
    /// // or
    ///
    /// ```rust
    /// # let res: Result<usize, ()> = Ok(1);
    ///
    /// // Bad
    /// res.expect("one");
    ///
    /// // Good
    /// res?;
    /// # Ok::<(), ()>(())
    /// ```
    pub EXPECT_USED,
    restriction,
    "using `.expect()` on `Result` or `Option`, which might be better handled"
}

/// lint use of `expect()` for `Option`s and `Result`s
pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, recv: &hir::Expr<'_>) {
    let obj_ty = cx.typeck_results().expr_ty(recv).peel_refs();

    let mess = if is_type_diagnostic_item(cx, obj_ty, sym::option_type) {
        Some((EXPECT_USED, "an Option", "None"))
    } else if is_type_diagnostic_item(cx, obj_ty, sym::result_type) {
        Some((EXPECT_USED, "a Result", "Err"))
    } else {
        None
    };

    if let Some((lint, kind, none_value)) = mess {
        span_lint_and_help(
            cx,
            lint,
            expr.span,
            &format!("used `expect()` on `{}` value", kind,),
            None,
            &format!("if this value is an `{}`, it will panic", none_value,),
        );
    }
}
