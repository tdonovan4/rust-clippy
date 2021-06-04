use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::ty::is_type_diagnostic_item;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for `.unwrap()` calls on `Option`s and on `Result`s.
    ///
    /// **Why is this bad?** It is better to handle the `None` or `Err` case,
    /// or at least call `.expect(_)` with a more helpful message. Still, for a lot of
    /// quick-and-dirty code, `unwrap` is a good choice, which is why this lint is
    /// `Allow` by default.
    ///
    /// `result.unwrap()` will let the thread panic on `Err` values.
    /// Normally, you want to implement more sophisticated error handling,
    /// and propagate errors upwards with `?` operator.
    ///
    /// Even if you want to panic on errors, not all `Error`s implement good
    /// messages on display. Therefore, it may be beneficial to look at the places
    /// where they may get displayed. Activate this lint to do just that.
    ///
    /// **Known problems:** None.
    ///
    /// **Examples:**
    /// ```rust
    /// # let opt = Some(1);
    ///
    /// // Bad
    /// opt.unwrap();
    ///
    /// // Good
    /// opt.expect("more helpful message");
    /// ```
    ///
    /// // or
    ///
    /// ```rust
    /// # let res: Result<usize, ()> = Ok(1);
    ///
    /// // Bad
    /// res.unwrap();
    ///
    /// // Good
    /// res.expect("more helpful message");
    /// ```
    pub UNWRAP_USED,
    restriction,
    "using `.unwrap()` on `Result` or `Option`, which should at least get a better message using `expect()`"
}

/// lint use of `unwrap()` for `Option`s and `Result`s
pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, recv: &hir::Expr<'_>) {
    let obj_ty = cx.typeck_results().expr_ty(recv).peel_refs();

    let mess = if is_type_diagnostic_item(cx, obj_ty, sym::option_type) {
        Some((UNWRAP_USED, "an Option", "None"))
    } else if is_type_diagnostic_item(cx, obj_ty, sym::result_type) {
        Some((UNWRAP_USED, "a Result", "Err"))
    } else {
        None
    };

    if let Some((lint, kind, none_value)) = mess {
        span_lint_and_help(
            cx,
            lint,
            expr.span,
            &format!("used `unwrap()` on `{}` value", kind,),
            None,
            &format!(
                "if you don't want to handle the `{}` case gracefully, consider \
                using `expect()` to provide a better panic message",
                none_value,
            ),
        );
    }
}
