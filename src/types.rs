use std::collections::HashMap;

type Id = u64;

const INDETERMINATE: Id = 0;
const FUNCTION: Id = 1;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TypeScope {
    Global,
    Module(String),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub id: Id, // going to need a hash instead if implementing precompiling libraries
    pub name: String,
    pub scope: TypeScope,
    // maybe TODO: (depending on how the type system will work)
    //pub base_class: Type,
    //pub interfaces: Vec<Type>
}

impl Type {
    pub fn new(id: Id, name: String, scope: TypeScope) -> Type {
        Type {
            id,
            name,
            scope,
        }
    }

    #[inline(always)]
    pub fn is_indeterminate(&self) -> bool {
        return self.id == INDETERMINATE;
    }
}

/*
pub struct FunctionPointer {
    pub return_type: Type,
    pub arguments: Vec<Type>,
}

pub enum ActualType {
    Type(Type),
    FunctionPointer(FunctionPointer)
}
 */

#[derive(Hash, Eq, PartialEq)]
pub struct TypeDef {
    pub name: String,
    pub scope: TypeScope,
}

pub struct TypeCollector {
    items: HashMap<TypeDef, Type>,
    items_id: HashMap<Id, Type>,
    id_counter: Id,
    // TODO: types should be a good candidate for Rc<Type>?
}

/*pub struct ClassReference {
    pub ident: String,
    pub class: String,
}

pub enum Reference {
    LocalScope(String),
    GlobalScope(String),
    //ClassScope(ClassReference),
    Indeterminate(String)
}

pub struct IndeterminateType {
    pub name: String,
    pub scope: TypeScope,
}

pub enum TypeVariant {
    Indeterminate(IndeterminateType),
    FromReference(Reference),
    Actual(ActualType)
}
 */

impl TypeCollector {
    // might remove Void later, feels a bit redundant to have both void and unit
    // if it's not useful for native interop, it's easy to just remove it/convert it to Unit later
    pub const VOID: &'static str = "Void";
    pub const ANY: &'static str = "Any";
    pub const STRING: &'static str = "String";
    pub const INT64: &'static str = "Int64";
    pub const DOUBLE: &'static str = "Double";
    pub const BOOLEAN: &'static str = "Boolean";
    pub const UNIT: &'static str = "Unit";

    pub fn new() -> TypeCollector {
        let mut collector = Self {
            items: HashMap::new(),
            items_id: HashMap::new(),
            id_counter: FUNCTION + 1,
        };
        collector.try_define_type(Self::VOID, TypeScope::Global).unwrap();
        collector.try_define_type(Self::ANY, TypeScope::Global).unwrap();
        collector.try_define_type(Self::STRING, TypeScope::Global).unwrap();
        collector.try_define_type(Self::INT64, TypeScope::Global).unwrap();
        collector.try_define_type(Self::DOUBLE, TypeScope::Global).unwrap();
        collector.try_define_type(Self::BOOLEAN, TypeScope::Global).unwrap();
        collector.try_define_type(Self::UNIT, TypeScope::Global).unwrap();
        collector
    }

    pub fn void(&self) -> Type {
        self.get_type(Self::VOID, &TypeScope::Global).clone()
    }

    pub fn any(&self) -> Type {
        self.get_type(Self::ANY, &TypeScope::Global).clone()
    }

    pub fn string(&self) -> Type {
        self.get_type(Self::STRING, &TypeScope::Global).clone()
    }

    pub fn int64(&self) -> Type {
        self.get_type(Self::INT64, &TypeScope::Global).clone()
    }

    pub fn double(&self) -> Type {
        self.get_type(Self::DOUBLE, &TypeScope::Global).clone()
    }

    pub fn boolean(&self) -> Type {
        self.get_type(Self::BOOLEAN, &TypeScope::Global).clone()
    }

    pub fn unit(&self) -> Type {
        self.get_type(Self::UNIT, &TypeScope::Global).clone()
    }

    fn new_indeterminate_type(name: &str, scope: TypeScope) -> Type {
        Type {
            id: INDETERMINATE,
            name: name.into(),
            scope,
        }
    }

    pub fn try_define_type(&mut self, name: &str, scope: TypeScope) -> Result<(), ()> {
        let type_def = TypeDef { name: name.into(), scope: scope.clone() };
        if self.items.contains_key(&type_def) {
            Err(())
        } else {
            let id = self.id_counter + 1;
            self.id_counter = id;
            let typ = Type { id, name: name.into(), scope };
            self.items.insert(type_def, typ.clone());
            self.items_id.insert(id, typ);
            Ok(())
        }
    }

    pub fn get_type(&self, name: &str, scope: &TypeScope) -> Type {
        // try to get closest type first, e.g. current module scope before global scope
        // TODO: reduce cloning
        match scope {
            TypeScope::Global => {
                self.items.get(&{ TypeDef { name: name.into(), scope: scope.clone() } })
                    .map(|t| t.clone())
                    .unwrap_or(Self::new_indeterminate_type(name, scope.clone()))
            }
            TypeScope::Module(_) => {
                self.items.get(&{ TypeDef { name: name.into(), scope: scope.clone() } })
                    .map_or_else(
                        || self.get_type(name, &TypeScope::Global),
                        |t| t.clone())
            }
        }
    }

    pub fn try_get_type_by_id(&self, id: Id) -> Option<Type> {
        self.items_id.get(&id)
            .map(|t| t.clone())
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    macro_rules! test_gen (
        ($typefn:ident, $type:ident) => {
            #[test]
            pub fn $type() {
                let types = TypeCollector::new();
                let t = types.$typefn();
                assert_eq!(t.name, TypeCollector::$type);
                assert_ne!(t.id, INDETERMINATE);
            }
        }
    );

    /* make sure the basic types are created */
    test_gen!(void,VOID);
    test_gen!(any,ANY);
    test_gen!(string,STRING);
    test_gen!(int64,INT64);
    test_gen!(double,DOUBLE);
    test_gen!(boolean,BOOLEAN);
    test_gen!(unit,UNIT);

    #[test]
    pub fn types_get_different_ids() {
        let types = TypeCollector::new();
        let t1 = types.int64();
        let t2 = types.string();
        assert_ne!(t1.id, t2.id);
    }

    #[test]
    pub fn get_indeterminate() {
        let types = TypeCollector::new();
        let t = types.get_type("doesnotexist", &TypeScope::Module("Hello".into()));
        assert_eq!(t.name, "doesnotexist");
        assert_eq!(t.id, INDETERMINATE);
    }
}