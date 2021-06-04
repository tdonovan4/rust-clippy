use super::utils::is_layout_incompatible;
use clippy_utils::diagnostics::span_lint;
use clippy_utils::{match_def_path, paths};
use rustc_hir::Expr;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_session::declare_tool_lint;

declare_clippy_lint! {
    /// **What it does:** Checks for transmutes between collections whose
    /// types have different ABI, size or alignment.
    ///
    /// **Why is this bad?** This is undefined behavior.
    ///
    /// **Known problems:** Currently, we cannot know whether a type is a
    /// collection, so we just lint the ones that come with `std`.
    ///
    /// **Example:**
    /// ```rust
    /// // different size, therefore likely out-of-bounds memory access
    /// // You absolutely do not want this in your code!
    /// unsafe {
    ///     std::mem::transmute::<_, Vec<u32>>(vec![2_u16])
    /// };
    /// ```
    ///
    /// You must always iterate, map and collect the values:
    ///
    /// ```rust
    /// vec![2_u16].into_iter().map(u32::from).collect::<Vec<_>>();
    /// ```
    pub UNSOUND_COLLECTION_TRANSMUTE,
    correctness,
    "transmute between collections of layout-incompatible types"
}

// used to check for UNSOUND_COLLECTION_TRANSMUTE
static COLLECTIONS: &[&[&str]] = &[
    &paths::VEC,
    &paths::VEC_DEQUE,
    &paths::BINARY_HEAP,
    &paths::BTREESET,
    &paths::BTREEMAP,
    &paths::HASHSET,
    &paths::HASHMAP,
];

/// Checks for `unsound_collection_transmute` lint.
/// Returns `true` if it's triggered, otherwise returns `false`.
pub(super) fn check<'tcx>(cx: &LateContext<'tcx>, e: &'tcx Expr<'_>, from_ty: Ty<'tcx>, to_ty: Ty<'tcx>) -> bool {
    match (&from_ty.kind(), &to_ty.kind()) {
        (ty::Adt(from_adt, from_substs), ty::Adt(to_adt, to_substs)) => {
            if from_adt.did != to_adt.did || !COLLECTIONS.iter().any(|path| match_def_path(cx, to_adt.did, path)) {
                return false;
            }
            if from_substs
                .types()
                .zip(to_substs.types())
                .any(|(from_ty, to_ty)| is_layout_incompatible(cx, from_ty, to_ty))
            {
                span_lint(
                    cx,
                    UNSOUND_COLLECTION_TRANSMUTE,
                    e.span,
                    &format!(
                        "transmute from `{}` to `{}` with mismatched layout is unsound",
                        from_ty, to_ty
                    ),
                );
                true
            } else {
                false
            }
        },
        _ => false,
    }
}
