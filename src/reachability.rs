use std::collections::HashSet;

#[derive(Debug, Default, Clone)]
struct OrderedSet<T> {
    v: Vec<T>,
    s: HashSet<T>,
}
impl<T: Eq + std::hash::Hash + Clone> OrderedSet<T> {
    fn insert(&mut self, value: T) -> bool {
        if self.s.insert(value.clone()) {
            self.v.push(value);
            true
        } else {
            false
        }
    }

    fn iter(&self) -> std::slice::Iter<T> {
        self.v.iter()
    }
}

type ID = usize;

#[derive(Debug, Default, Clone)]
pub struct Reachability {
    upsets: Vec<OrderedSet<ID>>,
    downsets: Vec<OrderedSet<ID>>,
}
impl Reachability {
    pub fn add_node(&mut self) -> ID {
        let i = self.upsets.len();
        self.upsets.push(Default::default());
        self.downsets.push(Default::default());
        i
    }

    pub fn add_edge(&mut self, lhs: ID, rhs: ID, out: &mut Vec<(ID, ID)>) {
        let mut work = vec![(lhs, rhs)];

        while let Some((lhs, rhs)) = work.pop() {
            // Insert returns false if the edge is already present
            if !self.downsets[lhs].insert(rhs) {
                continue;
            }
            self.upsets[rhs].insert(lhs);
            // Inform the caller that a new edge was added
            out.push((lhs, rhs));

            for &lhs2 in self.upsets[lhs].iter() {
                work.push((lhs2, rhs));
            }
            for &rhs2 in self.downsets[rhs].iter() {
                work.push((lhs, rhs2));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut r = Reachability::default();
        for i in 0..10 {
            r.add_node();
        }

        let mut out = Vec::new();
        r.add_edge(0, 8, &mut out);
        assert_eq!(out, vec![(0, 8)]);
        out.clear();
        r.add_edge(0, 8, &mut out);
        assert_eq!(out, vec![]);

        r.add_edge(0, 3, &mut out);
        r.add_edge(1, 3, &mut out);
        r.add_edge(2, 3, &mut out);
        r.add_edge(4, 5, &mut out);
        r.add_edge(4, 6, &mut out);
        r.add_edge(4, 7, &mut out);
        r.add_edge(6, 7, &mut out);
        r.add_edge(9, 1, &mut out);
        r.add_edge(9, 8, &mut out);

        out.clear();
        r.add_edge(3, 4, &mut out);

        let mut expected = Vec::new();
        for &lhs in &[0, 1, 2, 3, 9] {
            for &rhs in &[4, 5, 6, 7] {
                expected.push((lhs, rhs));
            }
        }

        out.sort_unstable();
        expected.sort_unstable();
        assert_eq!(out, expected);
    }
}
