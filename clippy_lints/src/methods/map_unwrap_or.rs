use clippy_utils::diagnostics::{span_lint, span_lint_and_sugg};
use clippy_utils::meets_msrv;
use clippy_utils::source::snippet;
use clippy_utils::ty::is_type_diagnostic_item;
use clippy_utils::usage::mutated_variables;
use rustc_errors::Applicability;
use rustc_hir as hir;
use rustc_lint::LateContext;
use rustc_semver::RustcVersion;
use rustc_session::declare_tool_lint;
use rustc_span::symbol::sym;

declare_clippy_lint! {
    /// **What it does:** Checks for usage of `option.map(_).unwrap_or(_)` or `option.map(_).unwrap_or_else(_)` or
    /// `result.map(_).unwrap_or_else(_)`.
    ///
    /// **Why is this bad?** Readability, these can be written more concisely (resp.) as
    /// `option.map_or(_, _)`, `option.map_or_else(_, _)` and `result.map_or_else(_, _)`.
    ///
    /// **Known problems:** The order of the arguments is not in execution order
    ///
    /// **Examples:**
    /// ```rust
    /// # let x = Some(1);
    ///
    /// // Bad
    /// x.map(|a| a + 1).unwrap_or(0);
    ///
    /// // Good
    /// x.map_or(0, |a| a + 1);
    /// ```
    ///
    /// // or
    ///
    /// ```rust
    /// # let x: Result<usize, ()> = Ok(1);
    /// # fn some_function(foo: ()) -> usize { 1 }
    ///
    /// // Bad
    /// x.map(|a| a + 1).unwrap_or_else(some_function);
    ///
    /// // Good
    /// x.map_or_else(some_function, |a| a + 1);
    /// ```
    pub MAP_UNWRAP_OR,
    pedantic,
    "using `.map(f).unwrap_or(a)` or `.map(f).unwrap_or_else(func)`, which are more succinctly expressed as `map_or(a, f)` or `map_or_else(a, f)`"
}

const MAP_UNWRAP_OR_MSRV: RustcVersion = RustcVersion::new(1, 41, 0);

/// lint use of `map().unwrap_or_else()` for `Option`s and `Result`s
/// Return true if lint triggered
pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &'tcx hir::Expr<'_>,
    recv: &'tcx hir::Expr<'_>,
    map_arg: &'tcx hir::Expr<'_>,
    unwrap_arg: &'tcx hir::Expr<'_>,
    msrv: Option<&RustcVersion>,
) -> bool {
    if !meets_msrv(msrv, &MAP_UNWRAP_OR_MSRV) {
        return false;
    }
    // lint if the caller of `map()` is an `Option`
    let is_option = is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(recv), sym::option_type);
    let is_result = is_type_diagnostic_item(cx, cx.typeck_results().expr_ty(recv), sym::result_type);

    if is_option || is_result {
        // Don't make a suggestion that may fail to compile due to mutably borrowing
        // the same variable twice.
        let map_mutated_vars = mutated_variables(recv, cx);
        let unwrap_mutated_vars = mutated_variables(unwrap_arg, cx);
        if let (Some(map_mutated_vars), Some(unwrap_mutated_vars)) = (map_mutated_vars, unwrap_mutated_vars) {
            if map_mutated_vars.intersection(&unwrap_mutated_vars).next().is_some() {
                return false;
            }
        } else {
            return false;
        }

        // lint message
        let msg = if is_option {
            "called `map(<f>).unwrap_or_else(<g>)` on an `Option` value. This can be done more directly by calling \
            `map_or_else(<g>, <f>)` instead"
        } else {
            "called `map(<f>).unwrap_or_else(<g>)` on a `Result` value. This can be done more directly by calling \
            `.map_or_else(<g>, <f>)` instead"
        };
        // get snippets for args to map() and unwrap_or_else()
        let map_snippet = snippet(cx, map_arg.span, "..");
        let unwrap_snippet = snippet(cx, unwrap_arg.span, "..");
        // lint, with note if neither arg is > 1 line and both map() and
        // unwrap_or_else() have the same span
        let multiline = map_snippet.lines().count() > 1 || unwrap_snippet.lines().count() > 1;
        let same_span = map_arg.span.ctxt() == unwrap_arg.span.ctxt();
        if same_span && !multiline {
            let var_snippet = snippet(cx, recv.span, "..");
            span_lint_and_sugg(
                cx,
                MAP_UNWRAP_OR,
                expr.span,
                msg,
                "try this",
                format!("{}.map_or_else({}, {})", var_snippet, unwrap_snippet, map_snippet),
                Applicability::MachineApplicable,
            );
            return true;
        } else if same_span && multiline {
            span_lint(cx, MAP_UNWRAP_OR, expr.span, msg);
            return true;
        }
    }

    false
}
