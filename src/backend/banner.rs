use figlet_rs::FIGfont;
use std::io::{Error, ErrorKind, Result as IOResult};

const VERSION: &str = "Ogu compiler version 0.3.0 (Ñeclito)";

pub fn akarru() -> IOResult<()> {
    let standard_font = FIGfont::standand().map_err(|s| Error::new(ErrorKind::Other, s))?;
    let figure = standard_font
        .convert("Akarrú")
        .ok_or_else(|| Error::new(ErrorKind::Other, "no banner"))?;
    println!("{}", figure);
    println!("{}", VERSION);
    Ok(())
}
