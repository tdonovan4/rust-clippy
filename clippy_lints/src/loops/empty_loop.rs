use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::{is_in_panic_handler, is_no_std_crate};

use rustc_hir::{Block, Expr};
use rustc_lint::LateContext;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for empty `loop` expressions.
    ///
    /// **Why is this bad?** These busy loops burn CPU cycles without doing
    /// anything. It is _almost always_ a better idea to `panic!` than to have
    /// a busy loop.
    ///
    /// If panicking isn't possible, think of the environment and either:
    ///   - block on something
    ///   - sleep the thread for some microseconds
    ///   - yield or pause the thread
    ///
    /// For `std` targets, this can be done with
    /// [`std::thread::sleep`](https://doc.rust-lang.org/std/thread/fn.sleep.html)
    /// or [`std::thread::yield_now`](https://doc.rust-lang.org/std/thread/fn.yield_now.html).
    ///
    /// For `no_std` targets, doing this is more complicated, especially because
    /// `#[panic_handler]`s can't panic. To stop/pause the thread, you will
    /// probably need to invoke some target-specific intrinsic. Examples include:
    ///   - [`x86_64::instructions::hlt`](https://docs.rs/x86_64/0.12.2/x86_64/instructions/fn.hlt.html)
    ///   - [`cortex_m::asm::wfi`](https://docs.rs/cortex-m/0.6.3/cortex_m/asm/fn.wfi.html)
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```no_run
    /// loop {}
    /// ```
    pub EMPTY_LOOP,
    style,
    "empty `loop {}`, which should block or sleep"
}

pub(super) fn check(cx: &LateContext<'tcx>, expr: &'tcx Expr<'_>, loop_block: &'tcx Block<'_>) {
    if loop_block.stmts.is_empty() && loop_block.expr.is_none() && !is_in_panic_handler(cx, expr) {
        let msg = "empty `loop {}` wastes CPU cycles";
        let help = if is_no_std_crate(cx) {
            "you should either use `panic!()` or add a call pausing or sleeping the thread to the loop body"
        } else {
            "you should either use `panic!()` or add `std::thread::sleep(..);` to the loop body"
        };
        span_lint_and_help(cx, EMPTY_LOOP, expr.span, msg, None, help);
    }
}
