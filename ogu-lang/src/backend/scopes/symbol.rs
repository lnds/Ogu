use crate::backend::scopes::types::Type;

pub(crate) trait Symbol: SymbolClone {
    fn get_name(&self) -> &str;
    fn get_type(&self) -> Option<&dyn Type>;
    fn set_type(&self, ty: &dyn Type);
}


pub(crate) trait SymbolClone {
    fn clone_box(&self) -> Box<dyn Symbol>;
}

impl<T> SymbolClone for T
    where
        T: 'static + Symbol + Clone,
{
    fn clone_box(&self) -> Box<dyn Symbol> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Symbol> {
    fn clone(&self) -> Box<dyn Symbol> {
        self.clone_box()
    }
}