# ogu-lang-compiler


Ogú is a dynamic programming language.

## Current Edition

This is the Ñeclito edition of this language. See below for details.

## Building Ogú

You will need rust 1.48 or superior.

You can build Ogú this way:

    $ cargo build --release

This will create the file `target/release/ogu`.

# Usage

    $ ogu

(After `cargo build`)

To run an Ogu Script you write it on a file with .ogu extension, and then passing the name to the ogu executable.

## Syntax

You can read about the Ogú syntax on the file OGU-0.3-en.md.

# About the name

Ogú is a comic character created by chilean illustrator [Themo Lobos](https://en.wikipedia.org/wiki/Themo_Lobos).

## Editions

The language will be released in several editions named after a character created by Themo Lobos.

These are the future editions:

- Plunke (0.1): (Deprecated) The first edition using Clojure runtime to interpret scripts written in a subset of the language.

- Ferrilo (0.2): (Deprecated) Second edition. It's a rewrite of the parser in Scala. There are many important changes in syntax. 
This release still depends on clojure runtime 1.10.0.

- Ñeclito (0.3): Third edition. Change and simplifies the syntax. This edition it's a more focused on functional paradigm.
The compiler it's written in Rust.

- Bromisnar (0.4)

- Cucufato (0.5)

- Cucalón (0.6)

- Alaraco (0.7)

- Guigá (0.8)

- Agú (0.9)

- Ogú (1.0)

## License

Copyright © 2011, 2017 Eduardo Díaz Cortés

Distributed under the BSD License, see LICENSE for details.
