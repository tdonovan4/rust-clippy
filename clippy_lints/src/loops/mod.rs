mod empty_loop;
mod explicit_counter_loop;
mod explicit_into_iter_loop;
mod explicit_iter_loop;
mod for_kv_map;
mod for_loops_over_fallibles;
mod iter_next_loop;
mod manual_flatten;
mod manual_memcpy;
mod mut_range_bound;
mod needless_collect;
mod needless_range_loop;
mod never_loop;
mod same_item_push;
mod single_element_loop;
mod utils;
mod while_immutable_condition;
mod while_let_loop;
mod while_let_on_iterator;

use clippy_utils::higher;
use rustc_hir::{Expr, ExprKind, LoopSource, Pat};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::declare_lint_pass;
use rustc_span::source_map::Span;
use utils::{get_span_of_entire_for_loop, make_iterator_snippet, IncrementVisitor, InitializeVisitor};

pub use empty_loop::EMPTY_LOOP;
pub use explicit_counter_loop::EXPLICIT_COUNTER_LOOP;
pub use explicit_into_iter_loop::EXPLICIT_INTO_ITER_LOOP;
pub use explicit_iter_loop::EXPLICIT_ITER_LOOP;
pub use for_kv_map::FOR_KV_MAP;
pub use for_loops_over_fallibles::FOR_LOOPS_OVER_FALLIBLES;
pub use iter_next_loop::ITER_NEXT_LOOP;
pub use manual_flatten::MANUAL_FLATTEN;
pub use manual_memcpy::MANUAL_MEMCPY;
pub use mut_range_bound::MUT_RANGE_BOUND;
pub use needless_collect::NEEDLESS_COLLECT;
pub use needless_range_loop::NEEDLESS_RANGE_LOOP;
pub use never_loop::NEVER_LOOP;
pub use same_item_push::SAME_ITEM_PUSH;
pub use single_element_loop::SINGLE_ELEMENT_LOOP;
pub use while_immutable_condition::WHILE_IMMUTABLE_CONDITION;
pub use while_let_loop::WHILE_LET_LOOP;
pub use while_let_on_iterator::WHILE_LET_ON_ITERATOR;

declare_lint_pass!(Loops => [
    MANUAL_MEMCPY,
    MANUAL_FLATTEN,
    NEEDLESS_RANGE_LOOP,
    EXPLICIT_ITER_LOOP,
    EXPLICIT_INTO_ITER_LOOP,
    ITER_NEXT_LOOP,
    FOR_LOOPS_OVER_FALLIBLES,
    WHILE_LET_LOOP,
    NEEDLESS_COLLECT,
    EXPLICIT_COUNTER_LOOP,
    EMPTY_LOOP,
    WHILE_LET_ON_ITERATOR,
    FOR_KV_MAP,
    NEVER_LOOP,
    MUT_RANGE_BOUND,
    WHILE_IMMUTABLE_CONDITION,
    SAME_ITEM_PUSH,
    SINGLE_ELEMENT_LOOP,
]);

impl<'tcx> LateLintPass<'tcx> for Loops {
    #[allow(clippy::too_many_lines)]
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx Expr<'_>) {
        if let Some((pat, arg, body, span)) = higher::for_loop(expr) {
            // we don't want to check expanded macros
            // this check is not at the top of the function
            // since higher::for_loop expressions are marked as expansions
            if body.span.from_expansion() {
                return;
            }
            check_for_loop(cx, pat, arg, body, expr, span);
        }

        // we don't want to check expanded macros
        if expr.span.from_expansion() {
            return;
        }

        // check for never_loop
        never_loop::check(cx, expr);

        // check for `loop { if let {} else break }` that could be `while let`
        // (also matches an explicit "match" instead of "if let")
        // (even if the "match" or "if let" is used for declaration)
        if let ExprKind::Loop(block, _, LoopSource::Loop, _) = expr.kind {
            // also check for empty `loop {}` statements, skipping those in #[panic_handler]
            empty_loop::check(cx, expr, block);
            while_let_loop::check(cx, expr, block);
        }

        while_let_on_iterator::check(cx, expr);

        if let Some((cond, body)) = higher::while_loop(expr) {
            while_immutable_condition::check(cx, cond, body);
        }

        needless_collect::check(expr, cx);
    }
}

fn check_for_loop<'tcx>(
    cx: &LateContext<'tcx>,
    pat: &'tcx Pat<'_>,
    arg: &'tcx Expr<'_>,
    body: &'tcx Expr<'_>,
    expr: &'tcx Expr<'_>,
    span: Span,
) {
    let is_manual_memcpy_triggered = manual_memcpy::check(cx, pat, arg, body, expr);
    if !is_manual_memcpy_triggered {
        needless_range_loop::check(cx, pat, arg, body, expr);
        explicit_counter_loop::check(cx, pat, arg, body, expr);
    }
    check_for_loop_arg(cx, pat, arg, expr);
    for_kv_map::check(cx, pat, arg, body, expr);
    mut_range_bound::check(cx, arg, body);
    single_element_loop::check(cx, pat, arg, body, expr);
    same_item_push::check(cx, pat, arg, body, expr);
    manual_flatten::check(cx, pat, arg, body, span);
}

fn check_for_loop_arg(cx: &LateContext<'_>, pat: &Pat<'_>, arg: &Expr<'_>, expr: &Expr<'_>) {
    let mut next_loop_linted = false; // whether or not ITER_NEXT_LOOP lint was used

    if let ExprKind::MethodCall(method, _, args, _) = arg.kind {
        // just the receiver, no arguments
        if args.len() == 1 {
            let method_name = &*method.ident.as_str();
            // check for looping over x.iter() or x.iter_mut(), could use &x or &mut x
            match method_name {
                "iter" | "iter_mut" => explicit_iter_loop::check(cx, args, arg, method_name),
                "into_iter" => {
                    explicit_iter_loop::check(cx, args, arg, method_name);
                    explicit_into_iter_loop::check(cx, args, arg);
                },
                "next" => {
                    next_loop_linted = iter_next_loop::check(cx, arg, expr);
                },
                _ => {},
            }
        }
    }

    if !next_loop_linted {
        for_loops_over_fallibles::check(cx, pat, arg);
    }
}
