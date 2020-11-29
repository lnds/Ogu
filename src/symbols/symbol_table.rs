use crate::parser::ast::module::body::Declaration;
use crate::parser::ast::module::Module;
use crate::symbols::raise_symbol_table_error;
use anyhow::Result;
use std::collections::HashMap;

pub(crate) struct SymbolTable {
    module_name: String,
    public_symbols: HashMap<String, Declaration>,
    private_symbols: HashMap<String, Declaration>,
}

impl SymbolTable {
    pub fn new(module: &mut Module) -> Result<Self> {
        let module_name = Self::extract_module_name(&module);
        let public_symbols = Self::extract_public_symbols(module, &module_name)?;
        let private_symbols = Self::extract_private_symbols(&module);
        Ok(Self {
            module_name,
            public_symbols,
            private_symbols,
        })
    }

    fn extract_module_name(module: &Module) -> String {
        module.get_module_name()
    }

    fn extract_public_symbols(
        module: &mut Module,
        module_name: &str,
    ) -> Result<HashMap<String, Declaration>> {
        let names = module.get_exposed_names();
        let mut result = HashMap::new();
        for name in names.iter() {
            match module.get_decl_by_name(name) {
                None => {
                    return raise_symbol_table_error(
                        "name not found in module",
                        name.to_string(),
                        module_name.to_string(),
                    )
                }
                Some(decl) => result.insert(name.to_string(), decl.clone()),
            };
        }
        Ok(result)
    }

    fn extract_private_symbols(_module: &Module) -> HashMap<String, Declaration> {
        todo!()
    }
}
