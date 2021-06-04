mod let_unit_value;
mod unit_arg;
mod unit_cmp;
mod utils;

use rustc_hir::{Expr, Stmt};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::declare_lint_pass;

pub use let_unit_value::LET_UNIT_VALUE;
pub use unit_arg::UNIT_ARG;
pub use unit_cmp::UNIT_CMP;

declare_lint_pass!(UnitTypes => [LET_UNIT_VALUE, UNIT_CMP, UNIT_ARG]);

impl LateLintPass<'_> for UnitTypes {
    fn check_stmt(&mut self, cx: &LateContext<'_>, stmt: &Stmt<'_>) {
        let_unit_value::check(cx, stmt);
    }

    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        unit_cmp::check(cx, expr);
        unit_arg::check(cx, expr);
    }
}
