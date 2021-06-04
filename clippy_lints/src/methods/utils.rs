use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::{implements_trait, is_copy, is_type_diagnostic_item};
use clippy_utils::{get_trait_def_id, paths};
use if_chain::if_chain;
use rustc_ast::ast;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_middle::ty::{self, Ty};
use rustc_span::symbol::sym;

pub(super) fn derefs_to_slice<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &'tcx hir::Expr<'tcx>,
    ty: Ty<'tcx>,
) -> Option<&'tcx hir::Expr<'tcx>> {
    fn may_slice<'a>(cx: &LateContext<'a>, ty: Ty<'a>) -> bool {
        match ty.kind() {
            ty::Slice(_) => true,
            ty::Adt(def, _) if def.is_box() => may_slice(cx, ty.boxed_ty()),
            ty::Adt(..) => is_type_diagnostic_item(cx, ty, sym::vec_type),
            ty::Array(_, size) => size
                .try_eval_usize(cx.tcx, cx.param_env)
                .map_or(false, |size| size < 32),
            ty::Ref(_, inner, _) => may_slice(cx, inner),
            _ => false,
        }
    }

    if let hir::ExprKind::MethodCall(path, _, args, _) = expr.kind {
        if path.ident.name == sym::iter && may_slice(cx, cx.typeck_results().expr_ty(&args[0])) {
            Some(&args[0])
        } else {
            None
        }
    } else {
        match ty.kind() {
            ty::Slice(_) => Some(expr),
            ty::Adt(def, _) if def.is_box() && may_slice(cx, ty.boxed_ty()) => Some(expr),
            ty::Ref(_, inner, _) => {
                if may_slice(cx, inner) {
                    Some(expr)
                } else {
                    None
                }
            },
            _ => None,
        }
    }
}

pub(super) fn get_hint_if_single_char_arg(
    cx: &LateContext<'_>,
    arg: &hir::Expr<'_>,
    applicability: &mut Applicability,
) -> Option<String> {
    if_chain! {
        if let hir::ExprKind::Lit(lit) = &arg.kind;
        if let ast::LitKind::Str(r, style) = lit.node;
        let string = r.as_str();
        if string.chars().count() == 1;
        then {
            let snip = snippet_with_applicability(cx, arg.span, &string, applicability);
            let ch = if let ast::StrStyle::Raw(nhash) = style {
                let nhash = nhash as usize;
                // for raw string: r##"a"##
                &snip[(nhash + 2)..(snip.len() - 1 - nhash)]
            } else {
                // for regular string: "a"
                &snip[1..(snip.len() - 1)]
            };
            let hint = format!("'{}'", if ch == "'" { "\\'" } else { ch });
            Some(hint)
        } else {
            None
        }
    }
}

/// Used for `lint_binary_expr_with_method_call`.
#[derive(Copy, Clone)]
pub(super) struct BinaryExprInfo<'a> {
    pub expr: &'a hir::Expr<'a>,
    pub chain: &'a hir::Expr<'a>,
    pub other: &'a hir::Expr<'a>,
    pub eq: bool,
}

#[rustfmt::skip]
pub(super) const PATTERN_METHODS: [(&str, usize); 17] = [
    ("contains", 1),
    ("starts_with", 1),
    ("ends_with", 1),
    ("find", 1),
    ("rfind", 1),
    ("split", 1),
    ("rsplit", 1),
    ("split_terminator", 1),
    ("rsplit_terminator", 1),
    ("splitn", 2),
    ("rsplitn", 2),
    ("matches", 1),
    ("rmatches", 1),
    ("match_indices", 1),
    ("rmatch_indices", 1),
    ("trim_start_matches", 1),
    ("trim_end_matches", 1),
];

#[derive(Clone, Copy, PartialEq, Debug)]
pub(super) enum SelfKind {
    Value,
    Ref,
    RefMut,
    No,
}

impl SelfKind {
    pub fn matches<'a>(self, cx: &LateContext<'a>, parent_ty: Ty<'a>, ty: Ty<'a>) -> bool {
        fn matches_value<'a>(cx: &LateContext<'a>, parent_ty: Ty<'_>, ty: Ty<'_>) -> bool {
            if ty == parent_ty {
                true
            } else if ty.is_box() {
                ty.boxed_ty() == parent_ty
            } else if is_type_diagnostic_item(cx, ty, sym::Rc) || is_type_diagnostic_item(cx, ty, sym::Arc) {
                if let ty::Adt(_, substs) = ty.kind() {
                    substs.types().next().map_or(false, |t| t == parent_ty)
                } else {
                    false
                }
            } else {
                false
            }
        }

        fn matches_ref<'a>(cx: &LateContext<'a>, mutability: hir::Mutability, parent_ty: Ty<'a>, ty: Ty<'a>) -> bool {
            if let ty::Ref(_, t, m) = *ty.kind() {
                return m == mutability && t == parent_ty;
            }

            let trait_path = match mutability {
                hir::Mutability::Not => &paths::ASREF_TRAIT,
                hir::Mutability::Mut => &paths::ASMUT_TRAIT,
            };

            let trait_def_id = match get_trait_def_id(cx, trait_path) {
                Some(did) => did,
                None => return false,
            };
            implements_trait(cx, ty, trait_def_id, &[parent_ty.into()])
        }

        match self {
            Self::Value => matches_value(cx, parent_ty, ty),
            Self::Ref => matches_ref(cx, hir::Mutability::Not, parent_ty, ty) || ty == parent_ty && is_copy(cx, ty),
            Self::RefMut => matches_ref(cx, hir::Mutability::Mut, parent_ty, ty),
            Self::No => ty != parent_ty,
        }
    }

    #[must_use]
    pub fn description(self) -> &'static str {
        match self {
            Self::Value => "`self` by value",
            Self::Ref => "`self` by reference",
            Self::RefMut => "`self` by mutable reference",
            Self::No => "no `self`",
        }
    }
}
