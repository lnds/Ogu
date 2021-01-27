use anyhow::{bail, Context, Result};
use figlet_rs::FIGfont;

const VERSION: &str = "Ogu compiler version 0.3.0 (Ñeclito)";

pub fn akarru() -> Result<()> {
    match FIGfont::standand() {
        Err(e) => {
            bail!("error: {}", e)
        }
        Ok(standard_font) => {
            let figure = standard_font
                .convert("Akarrú")
                .context("cant produce banner")?;
            println!("{}", figure);
            println!("{}", VERSION);
            Ok(())
        }
    }
}
