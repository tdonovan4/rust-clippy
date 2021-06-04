use clippy_utils::diagnostics::span_lint;
use clippy_utils::{match_def_path, match_qpath, paths};
use if_chain::if_chain;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for `MaybeUninit::uninit().assume_init()`.
    ///
    /// **Why is this bad?** For most types, this is undefined behavior.
    ///
    /// **Known problems:** For now, we accept empty tuples and tuples / arrays
    /// of `MaybeUninit`. There may be other types that allow uninitialized
    /// data, but those are not yet rigorously defined.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// // Beware the UB
    /// use std::mem::MaybeUninit;
    ///
    /// let _: usize = unsafe { MaybeUninit::uninit().assume_init() };
    /// ```
    ///
    /// Note that the following is OK:
    ///
    /// ```rust
    /// use std::mem::MaybeUninit;
    ///
    /// let _: [MaybeUninit<bool>; 5] = unsafe {
    ///     MaybeUninit::uninit().assume_init()
    /// };
    /// ```
    pub UNINIT_ASSUMED_INIT,
    correctness,
    "`MaybeUninit::uninit().assume_init()`"
}

/// lint for `MaybeUninit::uninit().assume_init()` (we already have the latter)
pub(super) fn check(cx: &LateContext<'_>, expr: &hir::Expr<'_>, recv: &hir::Expr<'_>) {
    if_chain! {
        if let hir::ExprKind::Call(callee, args) = recv.kind;
        if args.is_empty();
        if let hir::ExprKind::Path(ref path) = callee.kind;
        if match_qpath(path, &paths::MEM_MAYBEUNINIT_UNINIT);
        if !is_maybe_uninit_ty_valid(cx, cx.typeck_results().expr_ty_adjusted(expr));
        then {
            span_lint(
                cx,
                UNINIT_ASSUMED_INIT,
                expr.span,
                "this call for this type may be undefined behavior"
            );
        }
    }
}

fn is_maybe_uninit_ty_valid(cx: &LateContext<'_>, ty: Ty<'_>) -> bool {
    match ty.kind() {
        ty::Array(component, _) => is_maybe_uninit_ty_valid(cx, component),
        ty::Tuple(types) => types.types().all(|ty| is_maybe_uninit_ty_valid(cx, ty)),
        ty::Adt(adt, _) => match_def_path(cx, adt.did, &paths::MEM_MAYBEUNINIT),
        _ => false,
    }
}
