#![warn(clippy::circular_module_dependencies)]
#![allow(dead_code, unused_imports)]

// Test different ways to get a simple cycle between two modules

mod cycle_maker {
    use super::absolute_path::ABSOLUTE;
    use super::allow_lint::ALLOW;
    use super::glob_import::GLOB;
    use super::import_module::MODULE;
    use super::relative_path::RELATIVE;
    use super::use_declaration::USE;

    pub struct AStruct;
}

mod use_declaration {
    use super::cycle_maker::AStruct;

    pub const USE: bool = true;
}

mod relative_path {
    type Alias = super::cycle_maker::AStruct;

    pub const RELATIVE: bool = true;
}

mod absolute_path {
    type Alias = crate::cycle_maker::AStruct;

    pub const ABSOLUTE: bool = true;
}

mod import_module {
    // Should not create a cycle because simply referring to a module doesn't
    // create a dependency. This allows pub(in path) and pub(super) to work
    // without creating a cycle.
    use super::cycle_maker;

    pub const MODULE: bool = true;
}

mod pub_restriction {
    use self::pub_path::PATH;
    use self::pub_super::SUPER;

    mod pub_path {
        // Should not create a cycle
        pub(in pub_restriction) const PATH: bool = true;
    }

    mod pub_super {
        // Should not create a cycle
        pub(super) const SUPER: bool = true;
    }
}

mod glob_import {
    // Should not create a cycle because a glob import is just like importing
    // a module.
    use super::cycle_maker::*;

    pub const GLOB: bool = false;
}

mod allow_lint {
    // Should not warn about a cycle
    #[allow(clippy::circular_module_dependencies)]
    type Alias = super::cycle_maker::AStruct;

    pub const ALLOW: bool = false;
}

// Test parent-child cycle

mod parent {
    pub struct ParentStruct;

    mod child_no_cycle {
        pub struct ChildStruct;
    }

    mod child_cycle {
        pub type ParentAlias = super::ParentStruct;
    }

    // Fails, parent and child depends on each others
    use self::child_cycle::ParentAlias;
    // Fine since child doesn't know about parent
    use self::child_no_cycle::ChildStruct;
}

// Test multi-module cycle

mod module1 {
    use super::module3;

    type SomethingNewer = module3::SomethingNew;

    pub struct Something;
}

mod module2 {
    use super::module1::Something;

    pub struct SomethingElse;
}

mod module3 {
    // Fails, module2->module1->module3->module2->...
    pub type SomethingNew = super::module2::SomethingElse;
}

fn main() {}
