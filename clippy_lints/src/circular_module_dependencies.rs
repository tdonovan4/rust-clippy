use std::hash::Hash;
use std::{collections::VecDeque, hash::Hasher};

use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_data_structures::graph::implementation::{Graph, NodeIndex};
use rustc_hir::{
    def::{DefKind, Res},
    def_id::LocalDefId,
    HirId, Path,
};
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::ty::print;
use rustc_session::{declare_tool_lint, impl_lint_pass};

use clippy_utils::diagnostics::span_lint_and_help;

declare_clippy_lint! {
    /// **What it does:** Checks for circular dependencies between modules.
    ///
    /// **Why is this bad?** Circular dependencies between modules might make
    /// the codebase harder to understand because of the added cognitive
    /// complexity. Furthermore, cycles usually arise from high coupling between
    /// modules, which makes refactoring harder to do. There might also be an
    /// impact on the compile time of incremental builds since all modules in a
    /// cycle must be recompiled as soon as one them is modified.
    ///
    /// **Known problems:** None.
    ///
    /// **Example:**
    ///
    /// ```rust
    /// mod foo {
    ///     use super::bar::Something;
    ///
    ///     pub trait Important {}
    /// }
    ///
    /// mod bar {
    ///     use super::foo::Important;
    ///
    ///     pub struct Something;
    ///
    ///     impl Important for Something {}
    /// }
    /// ```
    /// Use instead:
    /// ```rust
    /// mod stuff {
    ///     pub trait Important {}
    /// }
    ///
    /// mod foo {
    ///     use super::bar::Something;
    /// }
    ///
    /// mod bar {
    ///     use super::stuff::Important;
    ///
    ///     pub struct Something;
    ///
    ///     impl Important for Something {}
    /// }
    /// ```
    pub CIRCULAR_MODULE_DEPENDENCIES,
    pedantic,
    "modules that shouldn't create a cycle"
}

#[derive(PartialEq, Clone, Copy)]
struct Edge {
    source: NodeIndex,
    target: NodeIndex,
}

impl Eq for Edge {}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Edge {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.source.node_id().hash(state);
        self.target.node_id().hash(state);
    }
}

/// A graph (ideally a DAG) of modules
struct ModGraph {
    /// Used to avoid duplicates and map a LocalDefId to a NodeIndex
    nodes_map: FxHashMap<LocalDefId, NodeIndex>,
    /// Serves solely to avoid duplicates.
    edges_set: FxHashSet<Edge>,
    graph: Graph<LocalDefId, ()>,
}

impl ModGraph {
    fn new() -> Self {
        Self {
            nodes_map: FxHashMap::default(),
            edges_set: FxHashSet::default(),
            graph: Graph::new(),
        }
    }

    /// This searches for a path from the starting node to the end node and
    /// returns it if found. This is implemented as a breadth-first search of
    /// the sucessors nodes.
    fn find_path(&self, start: NodeIndex, end: NodeIndex) -> Option<Vec<LocalDefId>> {
        let mut queue = VecDeque::new();
        queue.push_front(start);
        let mut visited = FxHashMap::default();
        // NodeIndex doesn't implement Eq and Hash and therefore cannot be used
        // as the HashMap key. The node's parent is also stored as a way to get
        // a path back to the start node.
        visited.insert(start.node_id(), None);
        while let Some(current_node) = queue.pop_front() {
            if current_node == end {
                let mut path_nodes = vec![current_node];

                // To get the path, we visit the parents starting from the end
                // node to the start node (who has no parent)
                while let Some(parent) = path_nodes
                    .last()
                    .and_then(|node| visited.get(&node.node_id()).copied().flatten())
                {
                    path_nodes.push(parent);
                }

                // The indexes are converted back LocalDefIds in the reverse
                // order so that the resulting Vec is in the right order.
                return Some(
                    path_nodes
                        .into_iter()
                        .rev()
                        .map(|node_idx| *self.graph.node_data(node_idx))
                        .collect(),
                );
            }

            for node in self.graph.successor_nodes(current_node) {
                visited.entry(node.node_id()).or_insert_with(|| {
                    queue.push_back(node);
                    Some(current_node)
                });
            }
        }

        None
    }

    fn add_node(&mut self, node: LocalDefId) -> NodeIndex {
        let index = self.graph.add_node(node);
        // We can later use this to convert the node back to the index
        self.nodes_map.insert(node, index);
        index
    }

    fn get_def_id_idx(&mut self, def_id: LocalDefId) -> NodeIndex {
        // If the node doesn't exist, we create it.
        self.nodes_map
            .get(&def_id)
            .copied()
            .unwrap_or_else(|| self.add_node(def_id))
    }

    /// Adds a new edge to the graph, possibly creating new nodes if they don't
    /// already exist.
    fn add_edge(&mut self, edge: Edge) -> bool {
        // This is to ignore intra-module relationships
        if edge.source == edge.target {
            return false;
        }

        // To avoid duplicate edges
        if self.edges_set.contains(&edge) {
            false
        } else {
            self.edges_set.insert(edge);
            self.graph.add_edge(edge.source, edge.target, ());

            true
        }
    }
}

fn get_target_module(cx: &LateContext<'_>, path: &Path<'_>) -> Option<LocalDefId> {
    if let Res::Def(kind, id) = path.res {
        // Importing a module is not considered a dependency on it's own.
        //
        // This allows using pub(in path) and pub(super) without creating a
        // cycle. It also means that glob imports are not considered a
        // dependency on their own. However, using them most likely mean that
        // another def is being imported, which would create a dependency.
        if let DefKind::Mod = kind {
            None
        } else {
            // We try to get the parent module
            id.as_local().map(|id| cx.tcx.parent_module_from_def_id(id))
        }
    } else {
        None
    }
}

fn gen_help_msg(cx: &LateContext<'_>, cyclic_path: &[LocalDefId]) -> String {
    let mut msg = String::new();
    for (i, module) in cyclic_path.iter().enumerate() {
        if i == 0 {
            // An arrow looping back to the first module
            msg += "+--->";
        } else {
            msg += "|  ->";
        }
        msg += format!(
            " {}\n",
            print::with_crate_prefix(|| cx.tcx.def_path_str(module.to_def_id()))
        )
        .as_str();

        // This is to continue the arrows on the next line
        if i == cyclic_path.len() - 1 {
            // This is the start of the final arrow, the one going
            // back to the start of the cycle
            msg += "+--+\n  this import creates the cycle";
        } else {
            msg += "|  |";
        }
        msg += "\n";
    }
    msg
}

pub struct CircularModuleDependencies {
    graph: ModGraph,
}

impl CircularModuleDependencies {
    #[must_use]
    pub fn new() -> Self {
        Self { graph: ModGraph::new() }
    }
}

impl_lint_pass!(CircularModuleDependencies => [CIRCULAR_MODULE_DEPENDENCIES]);

impl LateLintPass<'_> for CircularModuleDependencies {
    fn check_path(&mut self, cx: &LateContext<'_>, path: &Path<'_>, path_id: HirId) {
        // Used as the edge's source in the graph
        let current_module = cx.tcx.parent_module_from_def_id(path_id.owner);

        // We now need to get a module from the path resolution to use as the
        // target.
        //
        // If a LocalDefId couldn't be obtained (either because of a missing
        // path resolution or a failure to convert from a DefID to a
        // LocalDefId), an edge cannot be created and so we stop.
        if let Some(target_module) = get_target_module(cx, path) {
            let edge = Edge {
                source: self.graph.get_def_id_idx(current_module),
                target: self.graph.get_def_id_idx(target_module),
            };

            // The cycle check only runs on new edges
            if self.graph.add_edge(edge) {
                // Checking for cycle is simply a mather of traversing the
                // successors of the target looking for an already existing path
                // to the source. If found, there is a cycle.
                if let Some(cyclic_path) = self.graph.find_path(edge.target, edge.source) {
                    span_lint_and_help(
                        cx,
                        CIRCULAR_MODULE_DEPENDENCIES,
                        path.span,
                        "importing this creates a circular dependency",
                        None,
                        gen_help_msg(cx, &cyclic_path[..]).as_str(),
                    );
                }
            }
        }
    }
}
