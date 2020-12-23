use anyhow::{Context, Result};
use figlet_rs::FIGfont;
use crate::backend::errors::OguError;

const VERSION: &str = "Ogu compiler version 0.3.0 (Ñeclito)";

pub fn akarru() -> Result<()> {
    let standard_font = FIGfont::standand().map_err(OguError::FigfontError)?;
    let figure = standard_font
        .convert("Akarrú")
        .context("cant produce banner")?;
    println!("{}", figure);
    println!("{}", VERSION);
    Ok(())
}
