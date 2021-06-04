use super::utils::derefs_to_slice;
use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::paths;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::{is_type_diagnostic_item, match_type};
use rustc_errors::Applicability;
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;
use rustc_span::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for the use of `.iter().count()`.
    ///
    /// **Why is this bad?** `.len()` is more efficient and more
    /// readable.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// // Bad
    /// let some_vec = vec![0, 1, 2, 3];
    /// let _ = some_vec.iter().count();
    /// let _ = &some_vec[..].iter().count();
    ///
    /// // Good
    /// let some_vec = vec![0, 1, 2, 3];
    /// let _ = some_vec.len();
    /// let _ = &some_vec[..].len();
    /// ```
    pub ITER_COUNT,
    complexity,
    "replace `.iter().count()` with `.len()`"
}

pub(crate) fn check<'tcx>(cx: &LateContext<'tcx>, expr: &Expr<'_>, recv: &'tcx Expr<'tcx>, iter_method: &str) {
    let ty = cx.typeck_results().expr_ty(recv);
    let caller_type = if derefs_to_slice(cx, recv, ty).is_some() {
        "slice"
    } else if is_type_diagnostic_item(cx, ty, sym::vec_type) {
        "Vec"
    } else if is_type_diagnostic_item(cx, ty, sym::vecdeque_type) {
        "VecDeque"
    } else if is_type_diagnostic_item(cx, ty, sym::hashset_type) {
        "HashSet"
    } else if is_type_diagnostic_item(cx, ty, sym::hashmap_type) {
        "HashMap"
    } else if match_type(cx, ty, &paths::BTREEMAP) {
        "BTreeMap"
    } else if match_type(cx, ty, &paths::BTREESET) {
        "BTreeSet"
    } else if match_type(cx, ty, &paths::LINKED_LIST) {
        "LinkedList"
    } else if match_type(cx, ty, &paths::BINARY_HEAP) {
        "BinaryHeap"
    } else {
        return;
    };
    let mut applicability = Applicability::MachineApplicable;
    span_lint_and_sugg(
        cx,
        ITER_COUNT,
        expr.span,
        &format!("called `.{}().count()` on a `{}`", iter_method, caller_type),
        "try",
        format!(
            "{}.len()",
            snippet_with_applicability(cx, recv.span, "..", &mut applicability),
        ),
        applicability,
    );
}
