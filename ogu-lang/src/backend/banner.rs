use crate::backend::errors::OguError;
use anyhow::{Context, Error, Result};
use figlet_rs::FIGfont;

const VERSION: &str = "Ogu compiler version 0.3.0 (Ñeclito)";

pub fn akarru() -> Result<()> {
    let standard_font =
        FIGfont::standand().map_err(|s| Error::new(OguError::FigfontError).context(s))?;
    let figure = standard_font
        .convert("Akarrú")
        .context("cant produce banner")?;
    println!("{}", figure);
    println!("{}", VERSION);
    Ok(())
}
