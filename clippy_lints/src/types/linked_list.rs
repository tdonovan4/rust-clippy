use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::{match_def_path, paths};
use rustc_hir::{self as hir, def_id::DefId};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for usage of any `LinkedList`, suggesting to use a
    /// `Vec` or a `VecDeque` (formerly called `RingBuf`).
    ///
    /// **Why is this bad?** Gankro says:
    ///
    /// > The TL;DR of `LinkedList` is that it's built on a massive amount of
    /// pointers and indirection.
    /// > It wastes memory, it has terrible cache locality, and is all-around slow.
    /// `RingBuf`, while
    /// > "only" amortized for push/pop, should be faster in the general case for
    /// almost every possible
    /// > workload, and isn't even amortized at all if you can predict the capacity
    /// you need.
    /// >
    /// > `LinkedList`s are only really good if you're doing a lot of merging or
    /// splitting of lists.
    /// > This is because they can just mangle some pointers instead of actually
    /// copying the data. Even
    /// > if you're doing a lot of insertion in the middle of the list, `RingBuf`
    /// can still be better
    /// > because of how expensive it is to seek to the middle of a `LinkedList`.
    ///
    /// **Known problems:** False positives – the instances where using a
    /// `LinkedList` makes sense are few and far between, but they can still happen.
    ///
    /// **Example:**
    /// ```rust
    /// # use std::collections::LinkedList;
    /// let x: LinkedList<usize> = LinkedList::new();
    /// ```
    pub LINKEDLIST,
    pedantic,
    "usage of LinkedList, usually a vector is faster, or a more specialized data structure like a `VecDeque`"
}

pub(super) fn check(cx: &LateContext<'_>, hir_ty: &hir::Ty<'_>, def_id: DefId) -> bool {
    if match_def_path(cx, def_id, &paths::LINKED_LIST) {
        span_lint_and_help(
            cx,
            LINKEDLIST,
            hir_ty.span,
            "you seem to be using a `LinkedList`! Perhaps you meant some other data structure?",
            None,
            "a `VecDeque` might work",
        );
        true
    } else {
        false
    }
}
