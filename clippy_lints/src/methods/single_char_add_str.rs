use crate::methods::{single_char_insert_string, single_char_push_string};
use clippy_utils::{match_def_path, paths};
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Warns when using `push_str`/`insert_str` with a single-character string literal
    /// where `push`/`insert` with a `char` would work fine.
    ///
    /// **Why is this bad?** It's less clear that we are pushing a single character.
    ///
    /// **Known problems:** None
    ///
    /// **Example:**
    /// ```rust
    /// let mut string = String::new();
    /// string.insert_str(0, "R");
    /// string.push_str("R");
    /// ```
    /// Could be written as
    /// ```rust
    /// let mut string = String::new();
    /// string.insert(0, 'R');
    /// string.push('R');
    /// ```
    pub SINGLE_CHAR_ADD_STR,
    style,
    "`push_str()` or `insert_str()` used with a single-character string literal as parameter"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, args: &[hir::Expr<'_>]) {
    if let Some(fn_def_id) = cx.typeck_results().type_dependent_def_id(expr.hir_id) {
        if match_def_path(cx, fn_def_id, &paths::PUSH_STR) {
            #[allow(clippy::circular_module_dependencies)]
            single_char_push_string::check(cx, expr, args);
        } else if match_def_path(cx, fn_def_id, &paths::INSERT_STR) {
            #[allow(clippy::circular_module_dependencies)]
            single_char_insert_string::check(cx, expr, args);
        }
    }
}
