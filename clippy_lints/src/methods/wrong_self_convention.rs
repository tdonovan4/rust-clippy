use crate::methods::SelfKind;
use clippy_utils::diagnostics::span_lint_and_help;
use clippy_utils::ty::is_copy;
use rustc_lint::LateContext;
use rustc_middle::ty::TyS;
use rustc_session::declare_tool_lint;
use rustc_span::source_map::Span;
use std::fmt;

declare_clippy_lint! {
    /// **What it does:** Checks for methods with certain name prefixes and which
    /// doesn't match how self is taken. The actual rules are:
    ///
    /// |Prefix |Postfix     |`self` taken           | `self` type  |
    /// |-------|------------|-----------------------|--------------|
    /// |`as_`  | none       |`&self` or `&mut self` | any          |
    /// |`from_`| none       | none                  | any          |
    /// |`into_`| none       |`self`                 | any          |
    /// |`is_`  | none       |`&self` or none        | any          |
    /// |`to_`  | `_mut`     |`&mut self`            | any          |
    /// |`to_`  | not `_mut` |`self`                 | `Copy`       |
    /// |`to_`  | not `_mut` |`&self`                | not `Copy`   |
    ///
    /// Note: Clippy doesn't trigger methods with `to_` prefix in:
    /// - Traits definition.
    /// Clippy can not tell if a type that implements a trait is `Copy` or not.
    /// - Traits implementation, when `&self` is taken.
    /// The method signature is controlled by the trait and often `&self` is required for all types that implement the trait
    /// (see e.g. the `std::string::ToString` trait).
    ///
    /// Please find more info here:
    /// https://rust-lang.github.io/api-guidelines/naming.html#ad-hoc-conversions-follow-as_-to_-into_-conventions-c-conv
    ///
    /// **Why is this bad?** Consistency breeds readability. If you follow the
    /// conventions, your users won't be surprised that they, e.g., need to supply a
    /// mutable reference to a `as_..` function.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    /// ```rust
    /// # struct X;
    /// impl X {
    ///     fn as_str(self) -> &'static str {
    ///         // ..
    /// # ""
    ///     }
    /// }
    /// ```
    pub WRONG_SELF_CONVENTION,
    style,
    "defining a method named with an established prefix (like \"into_\") that takes `self` with the wrong convention"
}

declare_clippy_lint! {
    /// **What it does:** This is the same as
    /// [`wrong_self_convention`](#wrong_self_convention), but for public items.
    ///
    /// **Why is this bad?** See [`wrong_self_convention`](#wrong_self_convention).
    ///
    /// **Known problems:** Actually *renaming* the function may break clients if
    /// the function is part of the public interface. In that case, be mindful of
    /// the stability guarantees you've given your users.
    ///
    /// **Example:**
    /// ```rust
    /// # struct X;
    /// impl<'a> X {
    ///     pub fn as_str(self) -> &'a str {
    ///         "foo"
    ///     }
    /// }
    /// ```
    pub WRONG_PUB_SELF_CONVENTION,
    restriction,
    "defining a public method named with an established prefix (like \"into_\") that takes `self` with the wrong convention"
}

#[rustfmt::skip]
const CONVENTIONS: [(&[Convention], &[SelfKind]); 9] = [
    (&[Convention::Eq("new")], &[SelfKind::No]),
    (&[Convention::StartsWith("as_")], &[SelfKind::Ref, SelfKind::RefMut]),
    (&[Convention::StartsWith("from_")], &[SelfKind::No]),
    (&[Convention::StartsWith("into_")], &[SelfKind::Value]),
    (&[Convention::StartsWith("is_")], &[SelfKind::Ref, SelfKind::No]),
    (&[Convention::Eq("to_mut")], &[SelfKind::RefMut]),
    (&[Convention::StartsWith("to_"), Convention::EndsWith("_mut")], &[SelfKind::RefMut]),

    // Conversion using `to_` can use borrowed (non-Copy types) or owned (Copy types).
    // Source: https://rust-lang.github.io/api-guidelines/naming.html#ad-hoc-conversions-follow-as_-to_-into_-conventions-c-conv
    (&[Convention::StartsWith("to_"), Convention::NotEndsWith("_mut"), Convention::IsSelfTypeCopy(false), 
    Convention::IsTraitItem(false)], &[SelfKind::Ref]),
    (&[Convention::StartsWith("to_"), Convention::NotEndsWith("_mut"), Convention::IsSelfTypeCopy(true), 
    Convention::IsTraitItem(false), Convention::ImplementsTrait(false)], &[SelfKind::Value]),
];

enum Convention {
    Eq(&'static str),
    StartsWith(&'static str),
    EndsWith(&'static str),
    NotEndsWith(&'static str),
    IsSelfTypeCopy(bool),
    ImplementsTrait(bool),
    IsTraitItem(bool),
}

impl Convention {
    #[must_use]
    fn check<'tcx>(
        &self,
        cx: &LateContext<'tcx>,
        self_ty: &'tcx TyS<'tcx>,
        other: &str,
        implements_trait: bool,
        is_trait_item: bool,
    ) -> bool {
        match *self {
            Self::Eq(this) => this == other,
            Self::StartsWith(this) => other.starts_with(this) && this != other,
            Self::EndsWith(this) => other.ends_with(this) && this != other,
            Self::NotEndsWith(this) => !Self::EndsWith(this).check(cx, self_ty, other, implements_trait, is_trait_item),
            Self::IsSelfTypeCopy(is_true) => is_true == is_copy(cx, self_ty),
            Self::ImplementsTrait(is_true) => is_true == implements_trait,
            Self::IsTraitItem(is_true) => is_true == is_trait_item,
        }
    }
}

impl fmt::Display for Convention {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match *self {
            Self::Eq(this) => format!("`{}`", this).fmt(f),
            Self::StartsWith(this) => format!("`{}*`", this).fmt(f),
            Self::EndsWith(this) => format!("`*{}`", this).fmt(f),
            Self::NotEndsWith(this) => format!("`~{}`", this).fmt(f),
            Self::IsSelfTypeCopy(is_true) => {
                format!("`self` type is{} `Copy`", if is_true { "" } else { " not" }).fmt(f)
            },
            Self::ImplementsTrait(is_true) => {
                let (negation, s_suffix) = if is_true { ("", "s") } else { (" does not", "") };
                format!("method{} implement{} a trait", negation, s_suffix).fmt(f)
            },
            Self::IsTraitItem(is_true) => {
                let suffix = if is_true { " is" } else { " is not" };
                format!("method{} a trait item", suffix).fmt(f)
            },
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    item_name: &str,
    is_pub: bool,
    self_ty: &'tcx TyS<'tcx>,
    first_arg_ty: &'tcx TyS<'tcx>,
    first_arg_span: Span,
    implements_trait: bool,
    is_trait_item: bool,
) {
    let lint = if is_pub {
        WRONG_PUB_SELF_CONVENTION
    } else {
        WRONG_SELF_CONVENTION
    };
    if let Some((conventions, self_kinds)) = &CONVENTIONS.iter().find(|(convs, _)| {
        convs
            .iter()
            .all(|conv| conv.check(cx, self_ty, item_name, implements_trait, is_trait_item))
    }) {
        if !self_kinds.iter().any(|k| k.matches(cx, self_ty, first_arg_ty)) {
            let suggestion = {
                if conventions.len() > 1 {
                    // Don't mention `NotEndsWith` when there is also `StartsWith` convention present
                    let cut_ends_with_conv = conventions.iter().any(|conv| matches!(conv, Convention::StartsWith(_)))
                        && conventions
                            .iter()
                            .any(|conv| matches!(conv, Convention::NotEndsWith(_)));

                    let s = conventions
                        .iter()
                        .filter_map(|conv| {
                            if (cut_ends_with_conv && matches!(conv, Convention::NotEndsWith(_)))
                                || matches!(conv, Convention::ImplementsTrait(_))
                                || matches!(conv, Convention::IsTraitItem(_))
                            {
                                None
                            } else {
                                Some(conv.to_string())
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(" and ");

                    format!("methods with the following characteristics: ({})", &s)
                } else {
                    format!("methods called {}", &conventions[0])
                }
            };

            span_lint_and_help(
                cx,
                lint,
                first_arg_span,
                &format!(
                    "{} usually take {}",
                    suggestion,
                    &self_kinds
                        .iter()
                        .map(|k| k.description())
                        .collect::<Vec<_>>()
                        .join(" or ")
                ),
                None,
                "consider choosing a less ambiguous name",
            );
        }
    }
}
