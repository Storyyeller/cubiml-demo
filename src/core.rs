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
    VFloat,
    VInt,
    VStr,
    VFunc { arg: Use, ret: Value },
    VObj { fields: HashMap<String, Value> },
    VCase { case: (String, Value) },
}
#[derive(Debug, Clone)]
enum UTypeHead {
    UBool,
    UFloat,
    UInt,
    UStr,
    UFunc { arg: Value, ret: Use },
    UObj { field: (String, Use) },
    UCase { cases: HashMap<String, Use> },
}

fn check_heads(lhs: &VTypeHead, rhs: &UTypeHead, out: &mut Vec<(Value, Use)>) -> Result<(), TypeError> {
    use UTypeHead::*;
    use VTypeHead::*;

    match (lhs, rhs) {
        (&VBool, &UBool) => Ok(()),
        (&VFloat, &UFloat) => Ok(()),
        (&VInt, &UInt) => Ok(()),
        (&VStr, &UStr) => Ok(()),

        (&VFunc { arg: arg1, ret: ret1 }, &UFunc { arg: arg2, ret: ret2 }) => {
            out.push((ret1, ret2));
            // flip the order since arguments are contravariant
            out.push((arg2, arg1));
            Ok(())
        }
        (&VObj { fields: ref fields1 }, &UObj { field: (ref name, rhs2) }) => {
            // Check if the accessed field is defined
            match fields1.get(name) {
                Some(&lhs2) => {
                    out.push((lhs2, rhs2));
                    Ok(())
                }
                None => Err(TypeError(format!("Missing field {}", name))),
            }
        }
        (&VCase { case: (ref name, lhs2) }, &UCase { cases: ref cases2 }) => {
            // Check if the right case is handled
            match cases2.get(name) {
                Some(&rhs2) => {
                    out.push((lhs2, rhs2));
                    Ok(())
                }
                None => Err(TypeError(format!("Unhandled case {}", name))),
            }
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
    r: reachability::Reachability,
    types: Vec<TypeNode>,
}
impl TypeCheckerCore {
    pub fn new() -> Self {
        Self {
            r: Default::default(),
            types: Vec::new(),
        }
    }

    pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError> {
        let mut pending_edges = vec![(lhs, rhs)];
        let mut type_pairs_to_check = Vec::new();
        while let Some((lhs, rhs)) = pending_edges.pop() {
            self.r.add_edge(lhs.0, rhs.0, &mut type_pairs_to_check);

            // Check if adding that edge resulted in any new type pairs needing to be checked
            while let Some((lhs, rhs)) = type_pairs_to_check.pop() {
                if let TypeNode::Value(lhs_head) = &self.types[lhs] {
                    if let TypeNode::Use(rhs_head) = &self.types[rhs] {
                        check_heads(lhs_head, rhs_head, &mut pending_edges)?;
                    }
                }
            }
        }
        assert!(pending_edges.is_empty() && type_pairs_to_check.is_empty());
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
    pub fn float(&mut self) -> Value {
        self.new_val(VTypeHead::VFloat)
    }
    pub fn int(&mut self) -> Value {
        self.new_val(VTypeHead::VInt)
    }
    pub fn str(&mut self) -> Value {
        self.new_val(VTypeHead::VStr)
    }

    pub fn bool_use(&mut self) -> Use {
        self.new_use(UTypeHead::UBool)
    }
    pub fn float_use(&mut self) -> Use {
        self.new_use(UTypeHead::UFloat)
    }
    pub fn int_use(&mut self) -> Use {
        self.new_use(UTypeHead::UInt)
    }
    pub fn str_use(&mut self) -> Use {
        self.new_use(UTypeHead::UStr)
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
    pub fn obj_use(&mut self, field: (String, Use)) -> Use {
        self.new_use(UTypeHead::UObj { field })
    }

    pub fn case(&mut self, case: (String, Value)) -> Value {
        self.new_val(VTypeHead::VCase { case })
    }
    pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use {
        let cases = cases.into_iter().collect();
        self.new_use(UTypeHead::UCase { cases })
    }
}
