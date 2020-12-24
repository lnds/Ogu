mod transpilers;

use crate::symbols::module::Module;

trait CodeGen {

    fn process(module: &Module);
}