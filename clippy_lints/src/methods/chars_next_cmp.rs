use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for usage of `.chars().next()` on a `str` to check
    /// if it starts with a given char.
    ///
    /// **Why is this bad?** Readability, this can be written more concisely as
    /// `_.starts_with(_)`.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let name = "foo";
    /// if name.chars().next() == Some('_') {};
    /// ```
    /// Could be written as
    /// ```rust
    /// let name = "foo";
    /// if name.starts_with('_') {};
    /// ```
    pub CHARS_NEXT_CMP,
    style,
    "using `.chars().next()` to check if a string starts with a char"
}

/// Checks for the `CHARS_NEXT_CMP` lint.
pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, info: &crate::methods::BinaryExprInfo<'_>) -> bool {
    crate::methods::chars_cmp::check(cx, info, &["chars", "next"], CHARS_NEXT_CMP, "starts_with")
}
