use clippy_utils::diagnostics::span_lint;
use if_chain::if_chain;
use rustc_hir::{Expr, ExprKind, MutTy, Mutability, TyKind, UnOp};
use rustc_lint::LateContext;
use rustc_middle::ty;
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for casts of `&T` to `&mut T` anywhere in the code.
    ///
    /// **Why is this bad?** It’s basically guaranteed to be undefined behaviour.
    /// `UnsafeCell` is the only way to obtain aliasable data that is considered
    /// mutable.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust,ignore
    /// fn x(r: &i32) {
    ///     unsafe {
    ///         *(r as *const _ as *mut _) += 1;
    ///     }
    /// }
    /// ```
    ///
    /// Instead consider using interior mutability types.
    ///
    /// ```rust
    /// use std::cell::UnsafeCell;
    ///
    /// fn x(r: &UnsafeCell<i32>) {
    ///     unsafe {
    ///         *r.get() += 1;
    ///     }
    /// }
    /// ```
    pub CAST_REF_TO_MUT,
    correctness,
    "a cast of reference to a mutable pointer"
}

pub(super) fn check(cx: &LateContext<'tcx>, expr: &'tcx Expr<'_>) {
    if_chain! {
        if let ExprKind::Unary(UnOp::Deref, e) = &expr.kind;
        if let ExprKind::Cast(e, t) = &e.kind;
        if let TyKind::Ptr(MutTy { mutbl: Mutability::Mut, .. }) = t.kind;
        if let ExprKind::Cast(e, t) = &e.kind;
        if let TyKind::Ptr(MutTy { mutbl: Mutability::Not, .. }) = t.kind;
        if let ty::Ref(..) = cx.typeck_results().node_type(e.hir_id).kind();
        then {
            span_lint(
                cx,
                CAST_REF_TO_MUT,
                expr.span,
                "casting `&T` to `&mut T` may cause undefined behavior, consider instead using an `UnsafeCell`",
            );
        }
    }
}
