use super::{get_span_of_entire_for_loop, make_iterator_snippet, IncrementVisitor, InitializeVisitor};
use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::{get_enclosing_block, is_integer_const};
use if_chain::if_chain;
use rustc_errors::Applicability;
use rustc_hir::intravisit::{walk_block, walk_expr};
use rustc_hir::{Expr, Pat};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks `for` loops over slices with an explicit counter
    /// and suggests the use of `.enumerate()`.
    ///
    /// **Why is it bad?** Using `.enumerate()` makes the intent more clear,
    /// declutters the code and may be faster in some instances.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// # let v = vec![1];
    /// # fn bar(bar: usize, baz: usize) {}
    /// let mut i = 0;
    /// for item in &v {
    ///     bar(i, *item);
    ///     i += 1;
    /// }
    /// ```
    /// Could be written as
    /// ```rust
    /// # let v = vec![1];
    /// # fn bar(bar: usize, baz: usize) {}
    /// for (i, item) in v.iter().enumerate() { bar(i, *item); }
    /// ```
    pub EXPLICIT_COUNTER_LOOP,
    complexity,
    "for-looping with an explicit counter when `_.enumerate()` would do"
}

// To trigger the EXPLICIT_COUNTER_LOOP lint, a variable must be
// incremented exactly once in the loop body, and initialized to zero
// at the start of the loop.
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    pat: &'tcx Pat<'_>,
    arg: &'tcx Expr<'_>,
    body: &'tcx Expr<'_>,
    expr: &'tcx Expr<'_>,
) {
    // Look for variables that are incremented once per loop iteration.
    let mut increment_visitor = IncrementVisitor::new(cx);
    walk_expr(&mut increment_visitor, body);

    // For each candidate, check the parent block to see if
    // it's initialized to zero at the start of the loop.
    if let Some(block) = get_enclosing_block(cx, expr.hir_id) {
        for id in increment_visitor.into_results() {
            let mut initialize_visitor = InitializeVisitor::new(cx, expr, id);
            walk_block(&mut initialize_visitor, block);

            if_chain! {
                if let Some((name, initializer)) = initialize_visitor.get_result();
                if is_integer_const(cx, initializer, 0);
                then {
                    let mut applicability = Applicability::MachineApplicable;

                    let for_span = get_span_of_entire_for_loop(expr);

                    span_lint_and_sugg(
                        cx,
                        EXPLICIT_COUNTER_LOOP,
                        for_span.with_hi(arg.span.hi()),
                        &format!("the variable `{}` is used as a loop counter", name),
                        "consider using",
                        format!(
                            "for ({}, {}) in {}.enumerate()",
                            name,
                            snippet_with_applicability(cx, pat.span, "item", &mut applicability),
                            make_iterator_snippet(cx, arg, &mut applicability),
                        ),
                        applicability,
                    );
                }
            }
        }
    }
}
