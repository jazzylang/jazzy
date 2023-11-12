/*
 * MIT License
 *
 * Copyright (c) 2023 Dylan Tuttle
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

pub enum PrimitiveClass {
    Number,
    Bool,
    String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct DataType {
    pub data_type: Type,
    pub type_label: String,
}

impl DataType {
    pub fn new(data_type: Type, type_label: &str) -> DataType {
        return DataType {
            data_type,
            type_label: String::from(type_label),
        };
    }

    pub fn size(&self) -> usize {
        match self.data_type {
            Type::Primitive(data_type) => match data_type {
                Primitive::I8 | Primitive::U8 | Primitive::Bool => return 8,
                Primitive::I16 | Primitive::U16 => return 16,
                Primitive::I32 | Primitive::U32 | Primitive::F32 => return 32,
                Primitive::I64 | Primitive::U64 | Primitive::F64 => return 64,
                Primitive::I128 | Primitive::U128 => return 128,
                Primitive::CompileTimeInt | Primitive::CompileTimeFloat => {
                    panic!("Compile time numbers do not have a size")
                }
                Primitive::String => panic!("Size of Primitive::String is unknown at compile time"),
            },
            Type::UnTyped => panic!("Cannot call size() on Type::UnTyped"),
        }
    }

    pub fn get_label(&self) -> &str {
        return &self.type_label;
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Type {
    Primitive(Primitive),
    UnTyped,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Primitive {
    CompileTimeInt,
    CompileTimeFloat,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    String,
}

impl Type {
    pub fn get_label(&self) -> &str {
        match self {
            Type::Primitive(primitive) => match primitive {
                Primitive::CompileTimeInt => "integer",
                Primitive::CompileTimeFloat => "floating-point number",
                Primitive::I8 => "i8",
                Primitive::I16 => "i16",
                Primitive::I32 => "i32",
                Primitive::I64 => "i64",
                Primitive::I128 => "i128",
                Primitive::U8 => "u8",
                Primitive::U16 => "u16",
                Primitive::U32 => "u32",
                Primitive::U64 => "u64",
                Primitive::U128 => "u128",
                Primitive::F32 => "f32",
                Primitive::F64 => "f64",
                Primitive::Bool => "bool",
                Primitive::String => "str",
            },
            Type::UnTyped => "UnTyped",
        }
    }
}

pub enum CanCast {
    Yes,
    No(String),
}
