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
enum VTypeHead {
    VBool,
    VFunc { arg: Use, ret: Value },
    VObj { fields: HashMap<String, Value> },
    VCase { cases: HashMap<String, Value> },
}
#[derive(Debug, Clone)]
enum UTypeHead {
    UBool,
    UFunc { arg: Value, ret: Use },
    UObj { fields: HashMap<String, Use> },
    UCase { cases: HashMap<String, Use> },
}

fn check_heads(lhs: &VTypeHead, rhs: &UTypeHead, out: &mut Vec<(Value, Use)>) -> Result<(), TypeError> {
    use UTypeHead::*;
    use VTypeHead::*;

    match (lhs, rhs) {
        (&VBool, &UBool) => Ok(()),
        (&VFunc { arg: arg1, ret: ret1 }, &UFunc { arg: arg2, ret: ret2 }) => {
            out.push((ret1, ret2));
            // flip the order since arguments are contravariant
            out.push((arg2, arg1));
            Ok(())
        }
        (&VObj { fields: ref fields1 }, &UObj { fields: ref fields2 }) => {
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
        (&VCase { cases: ref cases1 }, &UCase { cases: ref cases2 }) => {
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

#[derive(Debug, Clone)]
enum TypeNode {
    Var,
    Value(VTypeHead),
    Use(UTypeHead),
}
#[derive(Debug, Clone)]
pub struct TypeCheckerCore {
    pub r: reachability::Reachability,

    pending_edges: Vec<(Value, Use)>,

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
            self.r.add_edge(lhs.0, rhs.0, &mut type_pairs_to_check);

            // Check if adding that edge resulted in any new type pairs needing to be checked
            while let Some((lhs, rhs)) = type_pairs_to_check.pop() {
                if let TypeNode::Value(lhs_head) = &self.types[lhs] {
                    if let TypeNode::Use(rhs_head) = &self.types[rhs] {
                        check_heads(lhs_head, rhs_head, &mut self.pending_edges)?;
                    }
                }
            }
        }
        assert!(self.pending_edges.is_empty() && type_pairs_to_check.is_empty());
        Ok(())
    }

    fn new_val(&mut self, val_type: VTypeHead) -> Value {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Value(val_type));
        Value(i)
    }

    fn new_use(&mut self, constraint: UTypeHead) -> Use {
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
        self.new_val(VTypeHead::VBool)
    }
    pub fn bool_use(&mut self) -> Use {
        self.new_use(UTypeHead::UBool)
    }

    pub fn func(&mut self, arg: Use, ret: Value) -> Value {
        self.new_val(VTypeHead::VFunc { arg, ret })
    }
    pub fn func_use(&mut self, arg: Value, ret: Use) -> Use {
        self.new_use(UTypeHead::UFunc { arg, ret })
    }

    pub fn obj(&mut self, fields: Vec<(String, Value)>) -> Value {
        let fields = fields.into_iter().collect();
        self.new_val(VTypeHead::VObj { fields })
    }
    pub fn obj_use(&mut self, fields: Vec<(String, Use)>) -> Use {
        let fields = fields.into_iter().collect();
        self.new_use(UTypeHead::UObj { fields })
    }

    pub fn case(&mut self, cases: Vec<(String, Value)>) -> Value {
        let cases = cases.into_iter().collect();
        self.new_val(VTypeHead::VCase { cases })
    }
    pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use {
        let cases = cases.into_iter().collect();
        self.new_use(UTypeHead::UCase { cases })
    }

    pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError> {
        self.pending_edges.push((lhs, rhs));
        self.process_pending_edges()
    }
}
