use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet_with_applicability;
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::TyS;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for loops on `y.into_iter()` where `y` will do, and
    /// suggests the latter.
    ///
    /// **Why is this bad?** Readability.
    ///
    /// **Known problems:** None
    ///
    /// **Example:**
    /// ```rust
    /// # let y = vec![1];
    /// // with `y` a `Vec` or slice:
    /// for x in y.into_iter() {
    ///     // ..
    /// }
    /// ```
    /// can be rewritten to
    /// ```rust
    /// # let y = vec![1];
    /// for x in y {
    ///     // ..
    /// }
    /// ```
    pub EXPLICIT_INTO_ITER_LOOP,
    pedantic,
    "for-looping over `_.into_iter()` when `_` would do"
}

pub(super) fn check(cx: &LateContext<'_>, args: &'hir [Expr<'hir>], arg: &Expr<'_>) {
    let receiver_ty = cx.typeck_results().expr_ty(&args[0]);
    let receiver_ty_adjusted = cx.typeck_results().expr_ty_adjusted(&args[0]);
    if !TyS::same_type(receiver_ty, receiver_ty_adjusted) {
        return;
    }

    let mut applicability = Applicability::MachineApplicable;
    let object = snippet_with_applicability(cx, args[0].span, "_", &mut applicability);
    span_lint_and_sugg(
        cx,
        EXPLICIT_INTO_ITER_LOOP,
        arg.span,
        "it is more concise to loop over containers instead of using explicit \
            iteration methods",
        "to write this more concisely, try",
        object.to_string(),
        applicability,
    );
}
