use crate::backend::OguError;
use crate::parser::ast::module::body::Declaration;
use crate::parser::ast::module::Module;
use anyhow::{Context, Error, Result};
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

type SymHashTable = HashMap<String, Declaration>;

pub(crate) struct SymbolTable {
    module_name: String,
    public_symbols: SymHashTable,
    private_symbols: SymHashTable,
}

impl SymbolTable {
    pub fn new(module: &mut Module) -> Result<Self> {
        let module_name = Self::extract_module_name(&module);
        let (public_symbols, private_symbols) = Self::extract_symbols(module, &module_name)?;
        Ok(Self {
            module_name,
            public_symbols,
            private_symbols,
        })
    }

    fn extract_module_name(module: &Module) -> String {
        module.get_module_name()
    }

    fn extract_symbols(
        module: &mut Module,
        module_name: &str,
    ) -> Result<(SymHashTable, SymHashTable)> {
        let exposed_names: HashSet<String> =
            HashSet::from_iter(module.get_exposed_names().iter().cloned());
        let mut pub_decls = HashMap::new();
        let mut prv_decls = HashMap::new();
        for decl in module.get_decls().iter() {
            let name = decl.get_name();
            if exposed_names.contains(&name) {
                pub_decls.insert(name, decl.clone());
            } else {
                prv_decls.insert(name, decl.clone());
            }
        }
        let pub_names_found: HashSet<String> =
            HashSet::from_iter(pub_decls.keys().into_iter().cloned());
        let diff: HashSet<String> = exposed_names
            .difference(&pub_names_found)
            .cloned()
            .collect();
        if diff.is_empty() {
            Ok((pub_decls, prv_decls))
        } else {
            let not_found = diff.iter().cloned().collect::<Vec<String>>().join(",");
            Err(Error::new(OguError::SymbolTableError(format!(
                "Exposed identifiers not found: {}",
                not_found
            ))))
            .context(format!(
                "Exposed identifiers not found: {} in module {}",
                not_found, module_name
            ))
        }
    }
}
