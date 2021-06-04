mod borrowed_box;
mod box_vec;
mod linked_list;
mod option_option;
mod rc_buffer;
mod redundant_allocation;
mod type_complexity;
mod utils;
mod vec_box;

use rustc_hir as hir;
use rustc_hir::intravisit::FnKind;
use rustc_hir::{
    Body, FnDecl, FnRetTy, GenericArg, HirId, ImplItem, ImplItemKind, Item, ItemKind, Local, MutTy, QPath, TraitItem,
    TraitItemKind, TyKind,
};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::impl_lint_pass;
use rustc_span::source_map::Span;

pub use borrowed_box::BORROWED_BOX;
pub use box_vec::BOX_VEC;
pub use linked_list::LINKEDLIST;
pub use option_option::OPTION_OPTION;
pub use rc_buffer::RC_BUFFER;
pub use redundant_allocation::REDUNDANT_ALLOCATION;
pub use type_complexity::TYPE_COMPLEXITY;
pub use vec_box::VEC_BOX;

pub struct Types {
    vec_box_size_threshold: u64,
    type_complexity_threshold: u64,
}

impl_lint_pass!(Types => [BOX_VEC, VEC_BOX, OPTION_OPTION, LINKEDLIST, BORROWED_BOX, REDUNDANT_ALLOCATION, RC_BUFFER, TYPE_COMPLEXITY]);

impl<'tcx> LateLintPass<'tcx> for Types {
    fn check_fn(&mut self, cx: &LateContext<'_>, _: FnKind<'_>, decl: &FnDecl<'_>, _: &Body<'_>, _: Span, id: HirId) {
        let is_in_trait_impl = if let Some(hir::Node::Item(item)) = cx.tcx.hir().find(cx.tcx.hir().get_parent_item(id))
        {
            matches!(item.kind, ItemKind::Impl(hir::Impl { of_trait: Some(_), .. }))
        } else {
            false
        };

        self.check_fn_decl(
            cx,
            decl,
            CheckTyContext {
                is_in_trait_impl,
                ..CheckTyContext::default()
            },
        );
    }

    fn check_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx Item<'_>) {
        match item.kind {
            ItemKind::Static(ty, _, _) | ItemKind::Const(ty, _) => self.check_ty(cx, ty, CheckTyContext::default()),
            // functions, enums, structs, impls and traits are covered
            _ => (),
        }
    }

    fn check_impl_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx ImplItem<'_>) {
        match item.kind {
            ImplItemKind::Const(ty, _) | ImplItemKind::TyAlias(ty) => self.check_ty(
                cx,
                ty,
                CheckTyContext {
                    is_in_trait_impl: true,
                    ..CheckTyContext::default()
                },
            ),
            // methods are covered by check_fn
            ImplItemKind::Fn(..) => (),
        }
    }

    fn check_field_def(&mut self, cx: &LateContext<'_>, field: &hir::FieldDef<'_>) {
        self.check_ty(cx, field.ty, CheckTyContext::default());
    }

    fn check_trait_item(&mut self, cx: &LateContext<'_>, item: &TraitItem<'_>) {
        match item.kind {
            TraitItemKind::Const(ty, _) | TraitItemKind::Type(_, Some(ty)) => {
                self.check_ty(cx, ty, CheckTyContext::default())
            },
            TraitItemKind::Fn(ref sig, _) => self.check_fn_decl(cx, sig.decl, CheckTyContext::default()),
            TraitItemKind::Type(..) => (),
        }
    }

    fn check_local(&mut self, cx: &LateContext<'_>, local: &Local<'_>) {
        if let Some(ty) = local.ty {
            self.check_ty(
                cx,
                ty,
                CheckTyContext {
                    is_local: true,
                    ..CheckTyContext::default()
                },
            );
        }
    }
}

impl Types {
    pub fn new(vec_box_size_threshold: u64, type_complexity_threshold: u64) -> Self {
        Self {
            vec_box_size_threshold,
            type_complexity_threshold,
        }
    }

    fn check_fn_decl(&mut self, cx: &LateContext<'_>, decl: &FnDecl<'_>, context: CheckTyContext) {
        for input in decl.inputs {
            self.check_ty(cx, input, context);
        }

        if let FnRetTy::Return(ty) = decl.output {
            self.check_ty(cx, ty, context);
        }
    }

    /// Recursively check for `TypePass` lints in the given type. Stop at the first
    /// lint found.
    ///
    /// The parameter `is_local` distinguishes the context of the type.
    fn check_ty(&mut self, cx: &LateContext<'_>, hir_ty: &hir::Ty<'_>, mut context: CheckTyContext) {
        if hir_ty.span.from_expansion() {
            return;
        }

        if !context.is_nested_call && type_complexity::check(cx, hir_ty, self.type_complexity_threshold) {
            return;
        }

        // Skip trait implementations; see issue #605.
        if context.is_in_trait_impl {
            return;
        }

        match hir_ty.kind {
            TyKind::Path(ref qpath) if !context.is_local => {
                let hir_id = hir_ty.hir_id;
                let res = cx.qpath_res(qpath, hir_id);
                if let Some(def_id) = res.opt_def_id() {
                    let mut triggered = false;
                    triggered |= box_vec::check(cx, hir_ty, qpath, def_id);
                    triggered |= redundant_allocation::check(cx, hir_ty, qpath, def_id);
                    triggered |= rc_buffer::check(cx, hir_ty, qpath, def_id);
                    triggered |= vec_box::check(cx, hir_ty, qpath, def_id, self.vec_box_size_threshold);
                    triggered |= option_option::check(cx, hir_ty, qpath, def_id);
                    triggered |= linked_list::check(cx, hir_ty, def_id);

                    if triggered {
                        return;
                    }
                }
                match *qpath {
                    QPath::Resolved(Some(ty), p) => {
                        context.is_nested_call = true;
                        self.check_ty(cx, ty, context);
                        for ty in p.segments.iter().flat_map(|seg| {
                            seg.args
                                .as_ref()
                                .map_or_else(|| [].iter(), |params| params.args.iter())
                                .filter_map(|arg| match arg {
                                    GenericArg::Type(ty) => Some(ty),
                                    _ => None,
                                })
                        }) {
                            self.check_ty(cx, ty, context);
                        }
                    },
                    QPath::Resolved(None, p) => {
                        context.is_nested_call = true;
                        for ty in p.segments.iter().flat_map(|seg| {
                            seg.args
                                .as_ref()
                                .map_or_else(|| [].iter(), |params| params.args.iter())
                                .filter_map(|arg| match arg {
                                    GenericArg::Type(ty) => Some(ty),
                                    _ => None,
                                })
                        }) {
                            self.check_ty(cx, ty, context);
                        }
                    },
                    QPath::TypeRelative(ty, seg) => {
                        context.is_nested_call = true;
                        self.check_ty(cx, ty, context);
                        if let Some(params) = seg.args {
                            for ty in params.args.iter().filter_map(|arg| match arg {
                                GenericArg::Type(ty) => Some(ty),
                                _ => None,
                            }) {
                                self.check_ty(cx, ty, context);
                            }
                        }
                    },
                    QPath::LangItem(..) => {},
                }
            },
            TyKind::Rptr(ref lt, ref mut_ty) => {
                context.is_nested_call = true;
                if !borrowed_box::check(cx, hir_ty, lt, mut_ty) {
                    self.check_ty(cx, mut_ty.ty, context);
                }
            },
            TyKind::Slice(ty) | TyKind::Array(ty, _) | TyKind::Ptr(MutTy { ty, .. }) => {
                context.is_nested_call = true;
                self.check_ty(cx, ty, context)
            },
            TyKind::Tup(tys) => {
                context.is_nested_call = true;
                for ty in tys {
                    self.check_ty(cx, ty, context);
                }
            },
            _ => {},
        }
    }
}

#[derive(Clone, Copy, Default)]
struct CheckTyContext {
    is_in_trait_impl: bool,
    is_local: bool,
    is_nested_call: bool,
}
