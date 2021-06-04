use crate::methods::chars_cmp;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for usage of `_.chars().last()` or
    /// `_.chars().next_back()` on a `str` to check if it ends with a given char.
    ///
    /// **Why is this bad?** Readability, this can be written more concisely as
    /// `_.ends_with(_)`.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// # let name = "_";
    ///
    /// // Bad
    /// name.chars().last() == Some('_') || name.chars().next_back() == Some('-');
    ///
    /// // Good
    /// name.ends_with('_') || name.ends_with('-');
    /// ```
    pub CHARS_LAST_CMP,
    style,
    "using `.chars().last()` or `.chars().next_back()` to check if a string ends with a char"
}

/// Checks for the `CHARS_LAST_CMP` lint.
pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, info: &crate::methods::BinaryExprInfo<'_>) -> bool {
    if chars_cmp::check(cx, info, &["chars", "last"], CHARS_LAST_CMP, "ends_with") {
        true
    } else {
        chars_cmp::check(cx, info, &["chars", "next_back"], CHARS_LAST_CMP, "ends_with")
    }
}
