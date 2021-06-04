mod bind_instead_of_map;
mod bytes_nth;
mod chars_cmp;
mod chars_cmp_with_unwrap;
mod chars_last_cmp;
mod chars_last_cmp_with_unwrap;
mod chars_next_cmp;
mod chars_next_cmp_with_unwrap;
mod clone_on_copy;
mod clone_on_ref_ptr;
mod expect_fun_call;
mod expect_used;
mod filetype_is_file;
mod filter_flat_map;
mod filter_map;
mod filter_map_flat_map;
mod filter_map_identity;
mod filter_map_map;
mod filter_map_next;
mod filter_next;
mod flat_map_identity;
mod from_iter_instead_of_collect;
mod get_unwrap;
mod implicit_clone;
mod inefficient_to_string;
mod inspect_for_each;
mod into_iter_on_ref;
mod iter_cloned_collect;
mod iter_count;
mod iter_next_slice;
mod iter_nth;
mod iter_nth_zero;
mod iter_skip_next;
mod iterator_step_by_zero;
mod manual_saturating_arithmetic;
mod map_collect_result_unit;
mod map_flatten;
mod map_unwrap_or;
mod ok_expect;
mod option_as_ref_deref;
mod option_map_or_none;
mod option_map_unwrap_or;
mod or_fun_call;
mod search_is_some;
mod single_char_add_str;
mod single_char_insert_string;
mod single_char_pattern;
mod single_char_push_string;
mod skip_while_next;
mod string_extend_chars;
mod suspicious_map;
mod uninit_assumed_init;
mod unnecessary_filter_map;
mod unnecessary_fold;
mod unnecessary_lazy_eval;
mod unwrap_used;
mod useless_asref;
mod utils;
mod wrong_self_convention;
mod zst_offset;

use bind_instead_of_map::BindInsteadOfMap;
use clippy_utils::diagnostics::{span_lint, span_lint_and_help};
use clippy_utils::ty::{contains_adt_constructor, contains_ty};
use clippy_utils::{contains_return, in_macro, iter_input_pats, return_ty};
use if_chain::if_chain;
use rustc_hir as hir;
use rustc_hir::def::Res;
use rustc_hir::{Expr, ExprKind, PrimTy, QPath, TraitItem, TraitItemKind};
use rustc_lint::{LateContext, LateLintPass, LintContext};
use rustc_middle::lint::in_external_macro;
use rustc_middle::ty::{self, TraitRef, TyS};
use rustc_semver::RustcVersion;
use rustc_session::{declare_tool_lint, impl_lint_pass};
use rustc_span::symbol::SymbolStr;
use rustc_span::{sym, Span};
use rustc_typeck::hir_ty_to_ty;

use utils::{BinaryExprInfo, SelfKind};

pub use bind_instead_of_map::BIND_INSTEAD_OF_MAP;
pub use bytes_nth::BYTES_NTH;
pub use chars_last_cmp::CHARS_LAST_CMP;
pub use chars_next_cmp::CHARS_NEXT_CMP;
pub use clone_on_copy::{CLONE_DOUBLE_REF, CLONE_ON_COPY};
pub use clone_on_ref_ptr::CLONE_ON_REF_PTR;
pub use expect_fun_call::EXPECT_FUN_CALL;
pub use expect_used::EXPECT_USED;
pub use filetype_is_file::FILETYPE_IS_FILE;
pub use filter_map::{FILTER_MAP, MANUAL_FILTER_MAP, MANUAL_FIND_MAP, OPTION_FILTER_MAP};
pub use filter_map_identity::FILTER_MAP_IDENTITY;
pub use filter_map_next::FILTER_MAP_NEXT;
pub use filter_next::FILTER_NEXT;
pub use flat_map_identity::FLAT_MAP_IDENTITY;
pub use from_iter_instead_of_collect::FROM_ITER_INSTEAD_OF_COLLECT;
pub use get_unwrap::GET_UNWRAP;
pub use implicit_clone::IMPLICIT_CLONE;
pub use inefficient_to_string::INEFFICIENT_TO_STRING;
pub use inspect_for_each::INSPECT_FOR_EACH;
pub use into_iter_on_ref::INTO_ITER_ON_REF;
pub use iter_cloned_collect::ITER_CLONED_COLLECT;
pub use iter_count::ITER_COUNT;
pub use iter_next_slice::ITER_NEXT_SLICE;
pub use iter_nth::ITER_NTH;
pub use iter_nth_zero::ITER_NTH_ZERO;
pub use iter_skip_next::ITER_SKIP_NEXT;
pub use iterator_step_by_zero::ITERATOR_STEP_BY_ZERO;
pub use manual_saturating_arithmetic::MANUAL_SATURATING_ARITHMETIC;
pub use map_collect_result_unit::MAP_COLLECT_RESULT_UNIT;
pub use map_flatten::MAP_FLATTEN;
pub use map_unwrap_or::MAP_UNWRAP_OR;
pub use ok_expect::OK_EXPECT;
pub use option_as_ref_deref::OPTION_AS_REF_DEREF;
pub use option_map_or_none::{OPTION_MAP_OR_NONE, RESULT_MAP_OR_INTO_OPTION};
pub use or_fun_call::OR_FUN_CALL;
pub use search_is_some::SEARCH_IS_SOME;
pub use single_char_add_str::SINGLE_CHAR_ADD_STR;
pub use single_char_pattern::SINGLE_CHAR_PATTERN;
pub use skip_while_next::SKIP_WHILE_NEXT;
pub use string_extend_chars::STRING_EXTEND_CHARS;
pub use suspicious_map::SUSPICIOUS_MAP;
pub use uninit_assumed_init::UNINIT_ASSUMED_INIT;
pub use unnecessary_filter_map::UNNECESSARY_FILTER_MAP;
pub use unnecessary_fold::UNNECESSARY_FOLD;
pub use unnecessary_lazy_eval::UNNECESSARY_LAZY_EVALUATIONS;
pub use unwrap_used::UNWRAP_USED;
pub use useless_asref::USELESS_ASREF;
pub use wrong_self_convention::{WRONG_PUB_SELF_CONVENTION, WRONG_SELF_CONVENTION};
pub use zst_offset::ZST_OFFSET;

declare_clippy_lint! {
    /// **What it does:** Checks for methods that should live in a trait
    /// implementation of a `std` trait (see [llogiq's blog
    /// post](http://llogiq.github.io/2015/07/30/traits.html) for further
    /// information) instead of an inherent implementation.
    ///
    /// **Why is this bad?** Implementing the traits improve ergonomics for users of
    /// the code, often with very little cost. Also people seeing a `mul(...)`
    /// method
    /// may expect `*` to work equally, so you should have good reason to disappoint
    /// them.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// struct X;
    /// impl X {
    ///     fn add(&self, other: &X) -> X {
    ///         // ..
    /// # X
    ///     }
    /// }
    /// ```
    pub SHOULD_IMPLEMENT_TRAIT,
    style,
    "defining a method that should be implementing a std trait"
}

declare_clippy_lint! {
    /// **What it does:** Checks for `new` not returning a type that contains `Self`.
    ///
    /// **Why is this bad?** As a convention, `new` methods are used to make a new
    /// instance of a type.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// In an impl block:
    /// ```rust
    /// # struct Foo;
    /// # struct NotAFoo;
    /// impl Foo {
    ///     fn new() -> NotAFoo {
    /// # NotAFoo
    ///     }
    /// }
    /// ```
    ///
    /// ```rust
    /// # struct Foo;
    /// struct Bar(Foo);
    /// impl Foo {
    ///     // Bad. The type name must contain `Self`
    ///     fn new() -> Bar {
    /// # Bar(Foo)
    ///     }
    /// }
    /// ```
    ///
    /// ```rust
    /// # struct Foo;
    /// # struct FooError;
    /// impl Foo {
    ///     // Good. Return type contains `Self`
    ///     fn new() -> Result<Foo, FooError> {
    /// # Ok(Foo)
    ///     }
    /// }
    /// ```
    ///
    /// Or in a trait definition:
    /// ```rust
    /// pub trait Trait {
    ///     // Bad. The type name must contain `Self`
    ///     fn new();
    /// }
    /// ```
    ///
    /// ```rust
    /// pub trait Trait {
    ///     // Good. Return type contains `Self`
    ///     fn new() -> Self;
    /// }
    /// ```
    pub NEW_RET_NO_SELF,
    style,
    "not returning type containing `Self` in a `new` method"
}

pub struct Methods {
    msrv: Option<RustcVersion>,
}

impl Methods {
    #[must_use]
    pub fn new(msrv: Option<RustcVersion>) -> Self {
        Self { msrv }
    }
}

impl_lint_pass!(Methods => [
    UNWRAP_USED,
    EXPECT_USED,
    SHOULD_IMPLEMENT_TRAIT,
    WRONG_SELF_CONVENTION,
    WRONG_PUB_SELF_CONVENTION,
    OK_EXPECT,
    MAP_UNWRAP_OR,
    RESULT_MAP_OR_INTO_OPTION,
    OPTION_MAP_OR_NONE,
    BIND_INSTEAD_OF_MAP,
    OR_FUN_CALL,
    EXPECT_FUN_CALL,
    CHARS_NEXT_CMP,
    CHARS_LAST_CMP,
    CLONE_ON_COPY,
    CLONE_ON_REF_PTR,
    CLONE_DOUBLE_REF,
    INEFFICIENT_TO_STRING,
    NEW_RET_NO_SELF,
    SINGLE_CHAR_PATTERN,
    SINGLE_CHAR_ADD_STR,
    SEARCH_IS_SOME,
    FILTER_NEXT,
    SKIP_WHILE_NEXT,
    FILTER_MAP,
    FILTER_MAP_IDENTITY,
    MANUAL_FILTER_MAP,
    MANUAL_FIND_MAP,
    OPTION_FILTER_MAP,
    FILTER_MAP_NEXT,
    FLAT_MAP_IDENTITY,
    MAP_FLATTEN,
    ITERATOR_STEP_BY_ZERO,
    ITER_NEXT_SLICE,
    ITER_COUNT,
    ITER_NTH,
    ITER_NTH_ZERO,
    BYTES_NTH,
    ITER_SKIP_NEXT,
    GET_UNWRAP,
    STRING_EXTEND_CHARS,
    ITER_CLONED_COLLECT,
    USELESS_ASREF,
    UNNECESSARY_FOLD,
    UNNECESSARY_FILTER_MAP,
    INTO_ITER_ON_REF,
    SUSPICIOUS_MAP,
    UNINIT_ASSUMED_INIT,
    MANUAL_SATURATING_ARITHMETIC,
    ZST_OFFSET,
    FILETYPE_IS_FILE,
    OPTION_AS_REF_DEREF,
    UNNECESSARY_LAZY_EVALUATIONS,
    MAP_COLLECT_RESULT_UNIT,
    FROM_ITER_INSTEAD_OF_COLLECT,
    INSPECT_FOR_EACH,
    IMPLICIT_CLONE
]);

/// Extracts a method call name, args, and `Span` of the method name.
fn method_call<'tcx>(recv: &'tcx hir::Expr<'tcx>) -> Option<(SymbolStr, &'tcx [hir::Expr<'tcx>], Span)> {
    if let ExprKind::MethodCall(path, span, args, _) = recv.kind {
        if !args.iter().any(|e| e.span.from_expansion()) {
            return Some((path.ident.name.as_str(), args, span));
        }
    }
    None
}

/// Same as `method_call` but the `SymbolStr` is dereferenced into a temporary `&str`
macro_rules! method_call {
    ($expr:expr) => {
        method_call($expr)
            .as_ref()
            .map(|&(ref name, args, span)| (&**name, args, span))
    };
}

impl<'tcx> LateLintPass<'tcx> for Methods {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx hir::Expr<'_>) {
        if in_macro(expr.span) {
            return;
        }

        check_methods(cx, expr, self.msrv.as_ref());

        match expr.kind {
            hir::ExprKind::Call(func, args) => {
                from_iter_instead_of_collect::check(cx, expr, args, &func.kind);
            },
            hir::ExprKind::MethodCall(method_call, ref method_span, args, _) => {
                or_fun_call::check(cx, expr, *method_span, &method_call.ident.as_str(), args);
                expect_fun_call::check(cx, expr, *method_span, &method_call.ident.as_str(), args);
                clone_on_copy::check(cx, expr, method_call.ident.name, args);
                clone_on_ref_ptr::check(cx, expr, method_call.ident.name, args);
                inefficient_to_string::check(cx, expr, method_call.ident.name, args);
                single_char_add_str::check(cx, expr, args);
                into_iter_on_ref::check(cx, expr, *method_span, method_call.ident.name, args);
                single_char_pattern::check(cx, expr, method_call.ident.name, args);
            },
            hir::ExprKind::Binary(op, lhs, rhs) if op.node == hir::BinOpKind::Eq || op.node == hir::BinOpKind::Ne => {
                let mut info = BinaryExprInfo {
                    expr,
                    chain: lhs,
                    other: rhs,
                    eq: op.node == hir::BinOpKind::Eq,
                };
                lint_binary_expr_with_method_call(cx, &mut info);
            },
            _ => (),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn check_impl_item(&mut self, cx: &LateContext<'tcx>, impl_item: &'tcx hir::ImplItem<'_>) {
        if in_external_macro(cx.sess(), impl_item.span) {
            return;
        }
        let name = impl_item.ident.name.as_str();
        let parent = cx.tcx.hir().get_parent_item(impl_item.hir_id());
        let item = cx.tcx.hir().expect_item(parent);
        let self_ty = cx.tcx.type_of(item.def_id);

        let implements_trait = matches!(item.kind, hir::ItemKind::Impl(hir::Impl { of_trait: Some(_), .. }));
        if_chain! {
            if let hir::ImplItemKind::Fn(ref sig, id) = impl_item.kind;
            if let Some(first_arg) = iter_input_pats(sig.decl, cx.tcx.hir().body(id)).next();

            let method_sig = cx.tcx.fn_sig(impl_item.def_id);
            let method_sig = cx.tcx.erase_late_bound_regions(method_sig);

            let first_arg_ty = &method_sig.inputs().iter().next();

            // check conventions w.r.t. conversion method names and predicates
            if let Some(first_arg_ty) = first_arg_ty;

            then {
                // if this impl block implements a trait, lint in trait definition instead
                if !implements_trait && cx.access_levels.is_exported(impl_item.hir_id()) {
                    // check missing trait implementations
                    for method_config in &TRAIT_METHODS {
                        if name == method_config.method_name &&
                            sig.decl.inputs.len() == method_config.param_count &&
                            method_config.output_type.matches(&sig.decl.output) &&
                            method_config.self_kind.matches(cx, self_ty, first_arg_ty) &&
                            fn_header_equals(method_config.fn_header, sig.header) &&
                            method_config.lifetime_param_cond(impl_item)
                        {
                            span_lint_and_help(
                                cx,
                                SHOULD_IMPLEMENT_TRAIT,
                                impl_item.span,
                                &format!(
                                    "method `{}` can be confused for the standard trait method `{}::{}`",
                                    method_config.method_name,
                                    method_config.trait_name,
                                    method_config.method_name
                                ),
                                None,
                                &format!(
                                    "consider implementing the trait `{}` or choosing a less ambiguous method name",
                                    method_config.trait_name
                                )
                            );
                        }
                    }
                }

                wrong_self_convention::check(
                    cx,
                    &name,
                    item.vis.node.is_pub(),
                    self_ty,
                    first_arg_ty,
                    first_arg.pat.span,
                    implements_trait,
                    false
                );
            }
        }

        // if this impl block implements a trait, lint in trait definition instead
        if implements_trait {
            return;
        }

        if let hir::ImplItemKind::Fn(_, _) = impl_item.kind {
            let ret_ty = return_ty(cx, impl_item.hir_id());

            // walk the return type and check for Self (this does not check associated types)
            if let Some(self_adt) = self_ty.ty_adt_def() {
                if contains_adt_constructor(ret_ty, self_adt) {
                    return;
                }
            } else if contains_ty(ret_ty, self_ty) {
                return;
            }

            // if return type is impl trait, check the associated types
            if let ty::Opaque(def_id, _) = *ret_ty.kind() {
                // one of the associated types must be Self
                for &(predicate, _span) in cx.tcx.explicit_item_bounds(def_id) {
                    if let ty::PredicateKind::Projection(projection_predicate) = predicate.kind().skip_binder() {
                        // walk the associated type and check for Self
                        if let Some(self_adt) = self_ty.ty_adt_def() {
                            if contains_adt_constructor(projection_predicate.ty, self_adt) {
                                return;
                            }
                        } else if contains_ty(projection_predicate.ty, self_ty) {
                            return;
                        }
                    }
                }
            }

            if name == "new" && !TyS::same_type(ret_ty, self_ty) {
                span_lint(
                    cx,
                    NEW_RET_NO_SELF,
                    impl_item.span,
                    "methods called `new` usually return `Self`",
                );
            }
        }
    }

    fn check_trait_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx TraitItem<'_>) {
        if in_external_macro(cx.tcx.sess, item.span) {
            return;
        }

        if_chain! {
            if let TraitItemKind::Fn(ref sig, _) = item.kind;
            if let Some(first_arg_ty) = sig.decl.inputs.iter().next();
            then {
                let first_arg_span = first_arg_ty.span;
                let first_arg_ty = hir_ty_to_ty(cx.tcx, first_arg_ty);
                let self_ty = TraitRef::identity(cx.tcx, item.def_id.to_def_id()).self_ty();
                wrong_self_convention::check(
                    cx,
                    &item.ident.name.as_str(),
                    false,
                    self_ty,
                    first_arg_ty,
                    first_arg_span,
                    false,
                    true
                );
            }
        }

        if_chain! {
            if item.ident.name == sym::new;
            if let TraitItemKind::Fn(_, _) = item.kind;
            let ret_ty = return_ty(cx, item.hir_id());
            let self_ty = TraitRef::identity(cx.tcx, item.def_id.to_def_id()).self_ty();
            if !contains_ty(ret_ty, self_ty);

            then {
                span_lint(
                    cx,
                    NEW_RET_NO_SELF,
                    item.span,
                    "methods called `new` usually return `Self`",
                );
            }
        }
    }

    extract_msrv_attr!(LateContext);
}

#[allow(clippy::too_many_lines)]
fn check_methods<'tcx>(cx: &LateContext<'tcx>, expr: &'tcx Expr<'_>, msrv: Option<&RustcVersion>) {
    if let Some((name, [recv, args @ ..], span)) = method_call!(expr) {
        match (name, args) {
            ("add" | "offset" | "sub" | "wrapping_offset" | "wrapping_add" | "wrapping_sub", [recv, _]) => {
                zst_offset::check(cx, expr, recv)
            },
            ("and_then", [arg]) => {
                let biom_option_linted = bind_instead_of_map::OptionAndThenSome::check(cx, expr, recv, arg);
                let biom_result_linted = bind_instead_of_map::ResultAndThenOk::check(cx, expr, recv, arg);
                if !biom_option_linted && !biom_result_linted {
                    unnecessary_lazy_eval::check(cx, expr, recv, arg, "and");
                }
            },
            ("as_mut", []) => useless_asref::check(cx, expr, "as_mut", recv),
            ("as_ref", []) => useless_asref::check(cx, expr, "as_ref", recv),
            ("assume_init", []) => uninit_assumed_init::check(cx, expr, recv),
            ("collect", []) => match method_call!(recv) {
                Some(("cloned", [recv2], _)) => iter_cloned_collect::check(cx, expr, recv2),
                Some(("map", [m_recv, m_arg], _)) => {
                    map_collect_result_unit::check(cx, expr, m_recv, m_arg, recv);
                },
                _ => {},
            },
            ("count", []) => match method_call!(recv) {
                Some((name @ ("into_iter" | "iter" | "iter_mut"), [recv2], _)) => {
                    iter_count::check(cx, expr, recv2, name);
                },
                Some(("map", [_, arg], _)) => suspicious_map::check(cx, expr, recv, arg),
                _ => {},
            },
            ("expect", [_]) => match method_call!(recv) {
                Some(("ok", [recv], _)) => ok_expect::check(cx, expr, recv),
                _ => expect_used::check(cx, expr, recv),
            },
            ("extend", [arg]) => string_extend_chars::check(cx, expr, recv, arg),
            ("filter_map", [arg]) => {
                unnecessary_filter_map::check(cx, expr, arg);
                filter_map_identity::check(cx, expr, arg, span);
            },
            ("flat_map", [flm_arg]) => match method_call!(recv) {
                Some(("filter", [_, _], _)) => filter_flat_map::check(cx, expr),
                Some(("filter_map", [_, _], _)) => filter_map_flat_map::check(cx, expr),
                _ => flat_map_identity::check(cx, expr, flm_arg, span),
            },
            ("flatten", []) => {
                if let Some(("map", [recv, map_arg], _)) = method_call!(recv) {
                    map_flatten::check(cx, expr, recv, map_arg);
                }
            },
            ("fold", [init, acc]) => unnecessary_fold::check(cx, expr, init, acc, span),
            ("for_each", [_]) => {
                if let Some(("inspect", [_, _], span2)) = method_call!(recv) {
                    inspect_for_each::check(cx, expr, span2);
                }
            },
            ("get_or_insert_with", [arg]) => unnecessary_lazy_eval::check(cx, expr, recv, arg, "get_or_insert"),
            ("is_file", []) => filetype_is_file::check(cx, expr, recv),
            ("is_none", []) => check_is_some_is_none(cx, expr, recv, false),
            ("is_some", []) => check_is_some_is_none(cx, expr, recv, true),
            ("map", [m_arg]) => {
                if let Some((name, [recv2, args @ ..], span2)) = method_call!(recv) {
                    match (name, args) {
                        ("as_mut", []) => option_as_ref_deref::check(cx, expr, recv2, m_arg, true, msrv),
                        ("as_ref", []) => option_as_ref_deref::check(cx, expr, recv2, m_arg, false, msrv),
                        ("filter", [f_arg]) => {
                            filter_map::check(cx, expr, recv2, f_arg, span2, recv, m_arg, span, false)
                        },
                        ("filter_map", [_]) => filter_map_map::check(cx, expr),
                        ("find", [f_arg]) => filter_map::check(cx, expr, recv2, f_arg, span2, recv, m_arg, span, true),
                        _ => {},
                    }
                }
            },
            ("map_or", [def, map]) => option_map_or_none::check(cx, expr, recv, def, map),
            ("next", []) => {
                if let Some((name, [recv, args @ ..], _)) = method_call!(recv) {
                    match (name, args) {
                        ("filter", [arg]) => filter_next::check(cx, expr, recv, arg),
                        ("filter_map", [arg]) => filter_map_next::check(cx, expr, recv, arg, msrv),
                        ("iter", []) => iter_next_slice::check(cx, expr, recv),
                        ("skip", [arg]) => iter_skip_next::check(cx, expr, recv, arg),
                        ("skip_while", [_]) => skip_while_next::check(cx, expr),
                        _ => {},
                    }
                }
            },
            ("nth", [n_arg]) => match method_call!(recv) {
                Some(("bytes", [recv2], _)) => bytes_nth::check(cx, expr, recv2, n_arg),
                Some(("iter", [recv2], _)) => iter_nth::check(cx, expr, recv2, recv, n_arg, false),
                Some(("iter_mut", [recv2], _)) => iter_nth::check(cx, expr, recv2, recv, n_arg, true),
                _ => iter_nth_zero::check(cx, expr, recv, n_arg),
            },
            ("ok_or_else", [arg]) => unnecessary_lazy_eval::check(cx, expr, recv, arg, "ok_or"),
            ("or_else", [arg]) => {
                if !bind_instead_of_map::ResultOrElseErrInfo::check(cx, expr, recv, arg) {
                    unnecessary_lazy_eval::check(cx, expr, recv, arg, "or");
                }
            },
            ("step_by", [arg]) => iterator_step_by_zero::check(cx, expr, arg),
            ("to_os_string", []) => implicit_clone::check(cx, expr, sym::OsStr),
            ("to_owned", []) => implicit_clone::check(cx, expr, sym::ToOwned),
            ("to_path_buf", []) => implicit_clone::check(cx, expr, sym::Path),
            ("to_vec", []) => implicit_clone::check(cx, expr, sym::slice),
            ("unwrap", []) => match method_call!(recv) {
                Some(("get", [recv, get_arg], _)) => get_unwrap::check(cx, expr, recv, get_arg, false),
                Some(("get_mut", [recv, get_arg], _)) => get_unwrap::check(cx, expr, recv, get_arg, true),
                _ => unwrap_used::check(cx, expr, recv),
            },
            ("unwrap_or", [u_arg]) => match method_call!(recv) {
                Some((arith @ ("checked_add" | "checked_sub" | "checked_mul"), [lhs, rhs], _)) => {
                    manual_saturating_arithmetic::check(cx, expr, lhs, rhs, u_arg, &arith["checked_".len()..]);
                },
                Some(("map", [m_recv, m_arg], span)) => {
                    option_map_unwrap_or::check(cx, expr, m_recv, m_arg, recv, u_arg, span)
                },
                _ => {},
            },
            ("unwrap_or_else", [u_arg]) => match method_call!(recv) {
                Some(("map", [recv, map_arg], _)) if map_unwrap_or::check(cx, expr, recv, map_arg, u_arg, msrv) => {},
                _ => unnecessary_lazy_eval::check(cx, expr, recv, u_arg, "unwrap_or"),
            },
            _ => {},
        }
    }
}

fn check_is_some_is_none(cx: &LateContext<'_>, expr: &Expr<'_>, recv: &Expr<'_>, is_some: bool) {
    if let Some((name @ ("find" | "position" | "rposition"), [f_recv, arg], span)) = method_call!(recv) {
        search_is_some::check(cx, expr, name, is_some, f_recv, arg, recv, span)
    }
}

/// Checks for the `CHARS_NEXT_CMP` and `CHARS_LAST_CMP` lints.
fn lint_binary_expr_with_method_call(cx: &LateContext<'_>, info: &mut BinaryExprInfo<'_>) {
    macro_rules! lint_with_both_lhs_and_rhs {
        ($func:expr, $cx:expr, $info:ident) => {
            if !$func($cx, $info) {
                ::std::mem::swap(&mut $info.chain, &mut $info.other);
                if $func($cx, $info) {
                    return;
                }
            }
        };
    }

    lint_with_both_lhs_and_rhs!(chars_next_cmp::check, cx, info);
    lint_with_both_lhs_and_rhs!(chars_last_cmp::check, cx, info);
    lint_with_both_lhs_and_rhs!(chars_next_cmp_with_unwrap::check, cx, info);
    lint_with_both_lhs_and_rhs!(chars_last_cmp_with_unwrap::check, cx, info);
}

const FN_HEADER: hir::FnHeader = hir::FnHeader {
    unsafety: hir::Unsafety::Normal,
    constness: hir::Constness::NotConst,
    asyncness: hir::IsAsync::NotAsync,
    abi: rustc_target::spec::abi::Abi::Rust,
};

struct ShouldImplTraitCase {
    trait_name: &'static str,
    method_name: &'static str,
    param_count: usize,
    fn_header: hir::FnHeader,
    // implicit self kind expected (none, self, &self, ...)
    self_kind: SelfKind,
    // checks against the output type
    output_type: OutType,
    // certain methods with explicit lifetimes can't implement the equivalent trait method
    lint_explicit_lifetime: bool,
}
impl ShouldImplTraitCase {
    const fn new(
        trait_name: &'static str,
        method_name: &'static str,
        param_count: usize,
        fn_header: hir::FnHeader,
        self_kind: SelfKind,
        output_type: OutType,
        lint_explicit_lifetime: bool,
    ) -> ShouldImplTraitCase {
        ShouldImplTraitCase {
            trait_name,
            method_name,
            param_count,
            fn_header,
            self_kind,
            output_type,
            lint_explicit_lifetime,
        }
    }

    fn lifetime_param_cond(&self, impl_item: &hir::ImplItem<'_>) -> bool {
        self.lint_explicit_lifetime
            || !impl_item.generics.params.iter().any(|p| {
                matches!(
                    p.kind,
                    hir::GenericParamKind::Lifetime {
                        kind: hir::LifetimeParamKind::Explicit
                    }
                )
            })
    }
}

#[rustfmt::skip]
const TRAIT_METHODS: [ShouldImplTraitCase; 30] = [
    ShouldImplTraitCase::new("std::ops::Add", "add",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::convert::AsMut", "as_mut",  1,  FN_HEADER,  SelfKind::RefMut,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::convert::AsRef", "as_ref",  1,  FN_HEADER,  SelfKind::Ref,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::ops::BitAnd", "bitand",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::BitOr", "bitor",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::BitXor", "bitxor",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::borrow::Borrow", "borrow",  1,  FN_HEADER,  SelfKind::Ref,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::borrow::BorrowMut", "borrow_mut",  1,  FN_HEADER,  SelfKind::RefMut,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::clone::Clone", "clone",  1,  FN_HEADER,  SelfKind::Ref,  OutType::Any, true),
    ShouldImplTraitCase::new("std::cmp::Ord", "cmp",  2,  FN_HEADER,  SelfKind::Ref,  OutType::Any, true),
    // FIXME: default doesn't work
    ShouldImplTraitCase::new("std::default::Default", "default",  0,  FN_HEADER,  SelfKind::No,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Deref", "deref",  1,  FN_HEADER,  SelfKind::Ref,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::ops::DerefMut", "deref_mut",  1,  FN_HEADER,  SelfKind::RefMut,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::ops::Div", "div",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Drop", "drop",  1,  FN_HEADER,  SelfKind::RefMut,  OutType::Unit, true),
    ShouldImplTraitCase::new("std::cmp::PartialEq", "eq",  2,  FN_HEADER,  SelfKind::Ref,  OutType::Bool, true),
    ShouldImplTraitCase::new("std::iter::FromIterator", "from_iter",  1,  FN_HEADER,  SelfKind::No,  OutType::Any, true),
    ShouldImplTraitCase::new("std::str::FromStr", "from_str",  1,  FN_HEADER,  SelfKind::No,  OutType::Any, true),
    ShouldImplTraitCase::new("std::hash::Hash", "hash",  2,  FN_HEADER,  SelfKind::Ref,  OutType::Unit, true),
    ShouldImplTraitCase::new("std::ops::Index", "index",  2,  FN_HEADER,  SelfKind::Ref,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::ops::IndexMut", "index_mut",  2,  FN_HEADER,  SelfKind::RefMut,  OutType::Ref, true),
    ShouldImplTraitCase::new("std::iter::IntoIterator", "into_iter",  1,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Mul", "mul",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Neg", "neg",  1,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::iter::Iterator", "next",  1,  FN_HEADER,  SelfKind::RefMut,  OutType::Any, false),
    ShouldImplTraitCase::new("std::ops::Not", "not",  1,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Rem", "rem",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Shl", "shl",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Shr", "shr",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
    ShouldImplTraitCase::new("std::ops::Sub", "sub",  2,  FN_HEADER,  SelfKind::Value,  OutType::Any, true),
];

#[derive(Clone, Copy)]
enum OutType {
    Unit,
    Bool,
    Any,
    Ref,
}

impl OutType {
    fn matches(self, ty: &hir::FnRetTy<'_>) -> bool {
        let is_unit = |ty: &hir::Ty<'_>| matches!(ty.kind, hir::TyKind::Tup(&[]));
        match (self, ty) {
            (Self::Unit, &hir::FnRetTy::DefaultReturn(_)) => true,
            (Self::Unit, &hir::FnRetTy::Return(ty)) if is_unit(ty) => true,
            (Self::Bool, &hir::FnRetTy::Return(ty)) if is_bool(ty) => true,
            (Self::Any, &hir::FnRetTy::Return(ty)) if !is_unit(ty) => true,
            (Self::Ref, &hir::FnRetTy::Return(ty)) => matches!(ty.kind, hir::TyKind::Rptr(_, _)),
            _ => false,
        }
    }
}

fn is_bool(ty: &hir::Ty<'_>) -> bool {
    if let hir::TyKind::Path(QPath::Resolved(_, path)) = ty.kind {
        matches!(path.res, Res::PrimTy(PrimTy::Bool))
    } else {
        false
    }
}

fn fn_header_equals(expected: hir::FnHeader, actual: hir::FnHeader) -> bool {
    expected.constness == actual.constness
        && expected.unsafety == actual.unsafety
        && expected.asyncness == actual.asyncness
}
