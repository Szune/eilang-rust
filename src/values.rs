#[derive(Debug)]
pub enum Value {
    Empty,
    Integer(i64),
    Double(f64),
    String(String),
    Bool(bool),
}

const _VALUE : Value = Value::Empty;

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

    pub fn empty() -> Value {
        _VALUE
    }
}