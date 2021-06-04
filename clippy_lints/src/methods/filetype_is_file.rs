use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::ty::match_type;
use clippy_utils::{get_parent_expr, paths};
use if_chain::if_chain;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::source_map::Span;

declare_clippy_lint! {
    /// **What it does:** Checks for `FileType::is_file()`.
    ///
    /// **Why is this bad?** When people testing a file type with `FileType::is_file`
    /// they are testing whether a path is something they can get bytes from. But
    /// `is_file` doesn't cover special file types in unix-like systems, and doesn't cover
    /// symlink in windows. Using `!FileType::is_dir()` is a better way to that intention.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// # || {
    /// let metadata = std::fs::metadata("foo.txt")?;
    /// let filetype = metadata.file_type();
    ///
    /// if filetype.is_file() {
    ///     // read file
    /// }
    /// # Ok::<_, std::io::Error>(())
    /// # };
    /// ```
    ///
    /// should be written as:
    ///
    /// ```rust
    /// # || {
    /// let metadata = std::fs::metadata("foo.txt")?;
    /// let filetype = metadata.file_type();
    ///
    /// if !filetype.is_dir() {
    ///     // read file
    /// }
    /// # Ok::<_, std::io::Error>(())
    /// # };
    /// ```
    pub FILETYPE_IS_FILE,
    restriction,
    "`FileType::is_file` is not recommended to test for readable file type"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, recv: &hir::Expr<'_>) {
    let ty = cx.typeck_results().expr_ty(recv);

    if !match_type(cx, ty, &paths::FILE_TYPE) {
        return;
    }

    let span: Span;
    let verb: &str;
    let lint_unary: &str;
    let help_unary: &str;
    if_chain! {
        if let Some(parent) = get_parent_expr(cx, expr);
        if let hir::ExprKind::Unary(op, _) = parent.kind;
        if op == hir::UnOp::Not;
        then {
            lint_unary = "!";
            verb = "denies";
            help_unary = "";
            span = parent.span;
        } else {
            lint_unary = "";
            verb = "covers";
            help_unary = "!";
            span = expr.span;
        }
    }
    let lint_msg = format!("`{}FileType::is_file()` only {} regular files", lint_unary, verb);
    let help_msg = format!("use `{}FileType::is_dir()` instead", help_unary);
    span_lint_and_help(cx, FILETYPE_IS_FILE, span, &lint_msg, None, &help_msg);
}
