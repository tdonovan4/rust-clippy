use crate::consts::{constant, Constant};
use clippy_utils::diagnostics::span_lint;
use clippy_utils::{method_chain_args, sext};
use if_chain::if_chain;
use rustc_hir::{Expr, ExprKind};
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for casts from a signed to an unsigned numerical
    /// type. In this case, negative values wrap around to large positive values,
    /// which can be quite surprising in practice. However, as the cast works as
    /// defined, this lint is `Allow` by default.
    ///
    /// **Why is this bad?** Possibly surprising results. You can activate this lint
    /// as a one-time check to see where numerical wrapping can arise.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// let y: i8 = -1;
    /// y as u128; // will return 18446744073709551615
    /// ```
    pub CAST_SIGN_LOSS,
    pedantic,
    "casts from signed types to unsigned types, e.g., `x as u32` where `x: i32`"
}

pub(super) fn check(cx: &LateContext<'_>, expr: &Expr<'_>, cast_op: &Expr<'_>, cast_from: Ty<'_>, cast_to: Ty<'_>) {
    if should_lint(cx, cast_op, cast_from, cast_to) {
        span_lint(
            cx,
            CAST_SIGN_LOSS,
            expr.span,
            &format!(
                "casting `{}` to `{}` may lose the sign of the value",
                cast_from, cast_to
            ),
        );
    }
}

fn should_lint(cx: &LateContext<'_>, cast_op: &Expr<'_>, cast_from: Ty<'_>, cast_to: Ty<'_>) -> bool {
    match (cast_from.is_integral(), cast_to.is_integral()) {
        (true, true) => {
            if !cast_from.is_signed() || cast_to.is_signed() {
                return false;
            }

            // Don't lint for positive constants.
            let const_val = constant(cx, cx.typeck_results(), cast_op);
            if_chain! {
                if let Some((Constant::Int(n), _)) = const_val;
                if let ty::Int(ity) = *cast_from.kind();
                if sext(cx.tcx, n, ity) >= 0;
                then {
                    return false;
                }
            }

            // Don't lint for the result of methods that always return non-negative values.
            if let ExprKind::MethodCall(path, _, _, _) = cast_op.kind {
                let mut method_name = path.ident.name.as_str();
                let allowed_methods = ["abs", "checked_abs", "rem_euclid", "checked_rem_euclid"];

                if_chain! {
                    if method_name == "unwrap";
                    if let Some(arglist) = method_chain_args(cast_op, &["unwrap"]);
                    if let ExprKind::MethodCall(inner_path, _, _, _) = &arglist[0][0].kind;
                    then {
                        method_name = inner_path.ident.name.as_str();
                    }
                }

                if allowed_methods.iter().any(|&name| method_name == name) {
                    return false;
                }
            }

            true
        },

        (false, true) => !cast_to.is_signed(),

        (_, _) => false,
    }
}
