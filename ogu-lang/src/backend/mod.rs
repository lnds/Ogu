use anyhow::Result;
use std::{env, fs};

pub mod banner;
pub mod compiler;
pub mod errors;
pub mod params;
pub(crate) mod scopes;

pub fn clean_ogu_dir() -> Result<()> {
    println!("cleaning ogu generated files...");
    let mut base = env::current_dir()?;
    base.push(".ogu");
    fs::remove_dir_all(base)?;
    Ok(())
}
