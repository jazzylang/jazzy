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

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    infrastructure::{error::ErrorReporter, log::Logger},
    parser::parser_data::{NodePointer, AST},
};

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
    pub fn new(data_type: Type) -> DataType {
        return DataType {
            data_type,
            type_label: data_type.get_label().to_owned(),
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

#[derive(PartialEq, Clone, Debug)]
pub enum Symbol {
    Variable(VariableSymbol),
}

impl Symbol {
    pub fn get_name(&self) -> &str {
        match self {
            Symbol::Variable(symbol) => &symbol.name,
        }
    }

    pub fn get_type(&self) -> DataType {
        match self {
            Symbol::Variable(symbol) => symbol.data_type.clone(),
        }
    }

    pub fn set_type(&mut self, data_type: DataType) {
        match self {
            Symbol::Variable(symbol) => symbol.data_type = data_type,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct VariableSymbol {
    pub name: String,
    pub data_type: DataType,
    pub mutable: bool,
}

impl VariableSymbol {
    pub fn new(name: String, mutable: bool) -> VariableSymbol {
        return VariableSymbol {
            name,
            data_type: DataType::new(Type::UnTyped),
            mutable,
        };
    }

    pub fn new_with_type(name: String, data_type: DataType, mutable: bool) -> VariableSymbol {
        return VariableSymbol {
            name,
            data_type,
            mutable,
        };
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    stack: Vec<HashMap<String, Rc<RefCell<Symbol>>>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        return SymbolTable {
            stack: vec![HashMap::new()],
        };
    }

    pub fn open_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn close_scope(&mut self) {
        self.stack.pop();
    }

    pub fn find_symbol(&self, name: &str) -> Option<&Rc<RefCell<Symbol>>> {
        // Look for the symbol with the given name,
        // starting in the topmost scope
        for scope in self.stack.iter().rev() {
            match scope.get(name) {
                Some(symbol) => return Some(symbol),
                None => {}
            }
        }

        // If we made it here, we couldn't find the symbol
        return None;
    }

    pub fn new_symbol(&mut self, name: String, symbol: Symbol) -> Rc<RefCell<Symbol>> {
        // Create a smart pointer to the new symbol
        let pointer = Rc::new(RefCell::new(symbol));

        // Insert the pointer into the topmost symbol table
        let top_scope_index = self.stack.len() - 1;
        self.stack[top_scope_index].insert(name, pointer.clone());

        return pointer;
    }
}

#[derive(Clone, Copy)]
pub enum Callback {
    GlobalDeclarations(GlobalDeclarations),
    IdentifiersPre(IdentifiersPre),
    IdentifiersPost(IdentifiersPost),
    TypeChecking(TypeChecking),
    MiscellaneousChecks(MiscellaneousChecks),
}

impl Callback {
    pub fn run(
        &mut self,
        node: NodePointer,
        ast: &mut AST,
        symbol_table: &mut SymbolTable,
        logger: &mut Logger,
        error: &mut ErrorReporter,
    ) {
        match self {
            Callback::GlobalDeclarations(callback) => {
                callback.run(node, ast, symbol_table, logger, error)
            }
            Callback::IdentifiersPre(callback) => {
                callback.run(node, ast, symbol_table, logger, error)
            }
            Callback::IdentifiersPost(callback) => {
                callback.run(node, ast, symbol_table, logger, error)
            }
            Callback::TypeChecking(callback) => {
                callback.run(node, ast, symbol_table, logger, error)
            }
            Callback::MiscellaneousChecks(callback) => {
                callback.run(node, ast, symbol_table, logger, error)
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct GlobalDeclarations {}

impl GlobalDeclarations {
    pub fn new() -> GlobalDeclarations {
        return GlobalDeclarations {};
    }
}

#[derive(Clone, Copy)]
pub struct IdentifiersPre {}

impl IdentifiersPre {
    pub fn new() -> IdentifiersPre {
        return IdentifiersPre {};
    }
}

#[derive(Clone, Copy)]
pub struct IdentifiersPost {}

impl IdentifiersPost {
    pub fn new() -> IdentifiersPost {
        return IdentifiersPost {};
    }
}

#[derive(Clone, Copy)]
pub struct TypeChecking {}

impl TypeChecking {
    pub fn new() -> TypeChecking {
        return TypeChecking {};
    }
}

#[derive(Clone, Copy)]
pub struct MiscellaneousChecks {}

impl MiscellaneousChecks {
    pub fn new() -> MiscellaneousChecks {
        return MiscellaneousChecks {};
    }
}
