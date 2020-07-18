use std::cell::RefCell;
use std::cmp::max;
use std::collections::{HashMap, HashSet};

type ID = usize;

#[derive(Debug, Default, Clone)]
pub struct Naive {
    upsets: Vec<HashSet<ID>>,
    downsets: Vec<HashSet<ID>>,
}
impl Naive {
    pub fn add_node(&mut self) -> ID {
        let i = self.upsets.len();

        let mut set = HashSet::with_capacity(1);
        set.insert(i);

        self.upsets.push(set.clone());
        self.downsets.push(set);
        i
    }

    pub fn add_edge(&mut self, lhs: ID, rhs: ID, out: &mut Vec<(ID, ID)>) {
        if self.downsets[lhs].contains(&rhs) {
            return;
        }

        // Get all ancestors of lhs, including lhs itself
        let mut lhs_set: Vec<ID> = self.upsets[lhs].iter().cloned().collect();
        lhs_set.sort_unstable();

        // Get all descendents of rhs, including rhs itself
        let mut rhs_set: Vec<ID> = self.downsets[rhs].iter().cloned().collect();
        rhs_set.sort_unstable();

        for &lhs2 in &lhs_set {
            for &rhs2 in &rhs_set {
                if self.downsets[lhs2].insert(rhs2) {
                    self.upsets[rhs2].insert(lhs2);
                    out.push((lhs2, rhs2));
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct TreeNodePtr(usize);

#[derive(Debug, Clone)]
struct TreeNode {
    id: ID,
    children: RefCell<Vec<TreeNodePtr>>,
}

#[derive(Debug, Default, Clone)]
pub struct Cubic {
    arena: Vec<TreeNode>,
    tree_roots: Vec<TreeNodePtr>,
    tree_maps: Vec<HashMap<ID, TreeNodePtr>>,
    reverse_map: Vec<Vec<ID>>,
}
impl Cubic {
    fn get(&self, p: TreeNodePtr) -> &TreeNode {
        &self.arena[p.0]
    }

    fn alloc(&mut self, id: ID) -> TreeNodePtr {
        let p = self.arena.len();
        self.arena.push(TreeNode {
            id,
            children: Default::default(),
        });
        TreeNodePtr(p)
    }

    fn insert_internal(
        &mut self,
        lhs_id: ID,
        lhs_ptr: TreeNodePtr,
        rhs_id: ID,
        rhs_ptr: TreeNodePtr,
        out: &mut Vec<(ID, ID)>,
    ) -> bool {
        assert!(lhs_ptr == *self.tree_maps[lhs_id].get(&self.get(lhs_ptr).id).unwrap());

        if self.tree_maps[lhs_id].contains_key(&rhs_id) {
            return false;
        }

        let new_ptr = self.alloc(rhs_id);
        self.tree_maps[lhs_id].insert(rhs_id, new_ptr);
        self.get(lhs_ptr).children.borrow_mut().push(new_ptr);
        out.push((lhs_id, rhs_id));

        let children = self.get(rhs_ptr).children.borrow().clone();
        for child_ptr in children {
            let child_id = self.get(child_ptr).id;
            self.insert_internal(lhs_id, new_ptr, child_id, child_ptr, out);
        }
        true
    }

    fn insert_toplevel(
        &mut self,
        lhs_id: ID,
        lhs_ptr: TreeNodePtr,
        rhs_id: ID,
        rhs_ptr: TreeNodePtr,
        out: &mut Vec<(ID, ID)>,
    ) {
        if self.insert_internal(lhs_id, lhs_ptr, rhs_id, rhs_ptr, out) {
            self.reverse_map[rhs_id].push(lhs_id);

            let parents = self.reverse_map[lhs_id].clone();
            for parent_id in parents {
                let parent_ptr = *self.tree_maps[parent_id].get(&lhs_id).unwrap();
                self.insert_toplevel(parent_id, parent_ptr, rhs_id, rhs_ptr, out);
            }
        }
    }

    pub fn add_node(&mut self) -> ID {
        let id = self.tree_roots.len();
        let p = self.alloc(id);
        let mut map = HashMap::with_capacity(1);
        map.insert(id, p);

        self.tree_roots.push(p);
        self.tree_maps.push(map);
        self.reverse_map.push(Vec::new());
        id
    }

    pub fn add_edge(&mut self, lhs: ID, rhs: ID, out: &mut Vec<(ID, ID)>) {
        self.insert_toplevel(lhs, self.tree_roots[lhs], rhs, self.tree_roots[rhs], out)
    }
}

// Run both engines in parallel to check for bugs
#[derive(Debug, Default, Clone)]
pub struct Both {
    a: Naive,
    b: Cubic,
}
impl Both {
    pub fn add_node(&mut self) -> ID {
        let r = self.a.add_node();
        assert!(r == self.b.add_node());
        r
    }

    pub fn add_edge(&mut self, lhs: ID, rhs: ID, out: &mut Vec<(ID, ID)>) {
        let mut ra = Vec::new();
        let mut rb = Vec::new();

        self.a.add_edge(lhs, rhs, &mut ra);
        self.b.add_edge(lhs, rhs, &mut rb);

        ra.sort_unstable();
        rb.sort_unstable();
        if &ra != &rb {
            dbg!(&ra);
            dbg!(&rb);
        }
        assert!(ra == rb);
        out.extend(ra);
    }
}
