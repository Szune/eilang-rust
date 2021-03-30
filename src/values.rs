/*
 * eilang is an experimental programming language, this is its compiler and interpreter.
 * Copyright (C) 2021  Carl Erik Patrik Iwarson
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#[derive(Debug)]
pub enum Value {
    Unit,
    Integer(i64),
    Double(f64),
    String(String),
    Bool(bool),
}

const _VALUE: Value = Value::Unit;

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
