#[derive(Debug)]
pub enum Value {
    Unit,
    Integer(i64),
    Double(f64),
    String(String),
    Bool(bool),
}

const _VALUE : Value = Value::Unit;

impl Value {
    pub fn int(value: i64) -> Value {
        Value::Integer(value)
    }

    pub fn double(value: f64) -> Value {
        Value::Double(value)
    }

    pub fn string(value: String) -> Value {
        Value::String(value)
    }

    pub fn unit() -> Value {
        _VALUE
    }

    pub fn bool(value: bool) -> Value {
        Value::Bool(value)
    }
}