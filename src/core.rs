use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::reachability;

type ID = usize;

#[derive(Copy, Clone, Debug)]
pub struct Value(ID);
#[derive(Copy, Clone, Debug)]
pub struct Use(ID);

#[derive(Debug)]
pub struct TypeError(String);
impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}
impl error::Error for TypeError {}

#[derive(Debug, Clone)]
enum TypeHead {
    Bool,
    Func { arg: ID, ret: ID },
    Obj { fields: HashMap<String, ID> },
    Case { cases: HashMap<String, ID> },
}
impl TypeHead {
    fn check(lhs: &Self, rhs: &Self, out: &mut Vec<(ID, ID)>) -> Result<(), TypeError> {
        use TypeHead::*;

        match (lhs, rhs) {
            (&Bool, &Bool) => Ok(()),
            (&Func { arg: arg1, ret: ret1 }, &Func { arg: arg2, ret: ret2 }) => {
                out.push((ret1, ret2));
                // flip the order since arguments are contrapositive
                out.push((arg2, arg1));
                Ok(())
            }
            (&Obj { fields: ref fields1 }, &Obj { fields: ref fields2 }) => {
                for (name, &rhs2) in fields2 {
                    match fields1.get(name) {
                        Some(&lhs2) => {
                            out.push((lhs2, rhs2));
                        }
                        None => {
                            return Err(TypeError(format!("Missing field {}", name)));
                        }
                    }
                }
                Ok(())
            }
            (&Case { cases: ref cases1 }, &Case { cases: ref cases2 }) => {
                for (name, &lhs2) in cases1 {
                    match cases2.get(name) {
                        Some(&rhs2) => {
                            out.push((lhs2, rhs2));
                        }
                        None => {
                            return Err(TypeError(format!("Unhandled case {}", name)));
                        }
                    }
                }
                Ok(())
            }
            _ => Err(TypeError("Unexpected types".to_string())),
        }
    }
}

#[derive(Debug, Clone)]
enum TypeNode {
    Var,
    Value(TypeHead),
    Use(TypeHead),
}
#[derive(Debug, Clone)]
pub struct TypeCheckerCore {
    pub r: reachability::Reachability,

    pending_edges: Vec<(ID, ID)>,

    types: Vec<TypeNode>,
}
impl TypeCheckerCore {
    pub fn new() -> Self {
        Self {
            r: Default::default(),
            pending_edges: Vec::new(),
            types: Vec::new(),
        }
    }

    fn process_pending_edges(&mut self) -> Result<(), TypeError> {
        let mut type_pairs_to_check = Vec::new();
        while let Some((lhs, rhs)) = self.pending_edges.pop() {
            self.r.add_edge(lhs, rhs, &mut type_pairs_to_check);

            // Check if adding that edge resulted in any new type pairs needing to be checked
            while let Some((lhs, rhs)) = type_pairs_to_check.pop() {
                if let TypeNode::Value(lhs_head) = &self.types[lhs] {
                    if let TypeNode::Use(rhs_head) = &self.types[rhs] {
                        TypeHead::check(lhs_head, rhs_head, &mut self.pending_edges)?;
                    }
                }
            }
        }
        assert!(self.pending_edges.is_empty() && type_pairs_to_check.is_empty());
        Ok(())
    }

    fn unwrap(&self, t: Value) -> ID {
        assert!(t.0 < self.types.len());
        t.0
    }
    fn unwrap2(&self, t: Use) -> ID {
        assert!(t.0 < self.types.len());
        t.0
    }

    fn new_val(&mut self, val_type: TypeHead) -> Value {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Value(val_type));
        Value(i)
    }

    fn new_use(&mut self, constraint: TypeHead) -> Use {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Use(constraint));
        Use(i)
    }

    pub fn var(&mut self) -> (Value, Use) {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Var);
        (Value(i), Use(i))
    }

    pub fn bool(&mut self) -> Value {
        self.new_val(TypeHead::Bool)
    }
    pub fn bool_use(&mut self) -> Use {
        self.new_use(TypeHead::Bool)
    }

    pub fn func(&mut self, arg: Use, ret: Value) -> Value {
        self.new_val(TypeHead::Func {
            arg: self.unwrap2(arg),
            ret: self.unwrap(ret),
        })
    }
    pub fn func_use(&mut self, arg: Value, ret: Use) -> Use {
        self.new_use(TypeHead::Func {
            arg: self.unwrap(arg),
            ret: self.unwrap2(ret),
        })
    }

    pub fn obj(&mut self, fields: Vec<(String, Value)>) -> Value {
        let fields = fields.into_iter().map(|(k, v)| (k, self.unwrap(v))).collect();
        self.new_val(TypeHead::Obj { fields })
    }
    pub fn obj_use(&mut self, fields: Vec<(String, Use)>) -> Use {
        let fields = fields.into_iter().map(|(k, v)| (k, self.unwrap2(v))).collect();
        self.new_use(TypeHead::Obj { fields })
    }

    pub fn case(&mut self, cases: Vec<(String, Value)>) -> Value {
        let cases = cases.into_iter().map(|(k, v)| (k, self.unwrap(v))).collect();
        self.new_val(TypeHead::Case { cases })
    }
    pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use {
        let cases = cases.into_iter().map(|(k, v)| (k, self.unwrap2(v))).collect();
        self.new_use(TypeHead::Case { cases })
    }

    pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError> {
        self.pending_edges.push((self.unwrap(lhs), self.unwrap2(rhs)));
        self.process_pending_edges()
    }
}
