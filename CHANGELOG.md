# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Added

- static type checking
- type inference
- true and false literal values
- curry functions
- algebraic effects
- macros

### Changed

- args pass
- not is an operator: !
- case expression accepts guards:
    case e of
       pattern | cond -> expr
- functions always have at leas one arg  


### Removed

- regular expression operators
- sets (${})
- remove !> >! >| and |< operators
- atoms (:atom)
- classes

## [0.2.5] - 2019-3-3

### Added

- add proxy
- add sync
- doto
- turtle graphics
- add **args**
- arguments parsing
- change bin/ogu bash script

### Fixed 

- bug on impor from jvm
- fix tabs on some demos
- Static method call
- dictionary expressions

### Changed

- major refactor of codegen to clojure

## [0.2.4] - 2019-2-26

### Changed

- all expressions are in separated parse objects
- more functional style for code of parser

## [0.2.3] - 2019-2-25

### Fixed

- restructure packages

## [0.2.2] - 2019-2-24

### Fixed 

- major refactoring
- morefunctional style
- remove duplicated code in parser

## [0.2.1] - 2019-2-22

### Fixed

- Refactor Parser
- Refactor Lexer
- remove duplicated code

## [0.2.0] - 2019-2-21

### Changed

- new parser in scala, but runtime still is clojure
- function definitions require def keyword
- val is marked deprecated, in next release will be removed
- many minors changes in syntax

### Removed

- begin/end, now you must indent like python/haskell.


## [0.1.5] - 2017-11-4

### Added

- more support for ADT (see demos/adt.ogu   )

## [0.1.4] - 2017-10-30

### Added

- bash command for ogu
- ogu.turtle module
- use sentence

## [0.1.3] - 2017-10-29

### Added

- regular expressions
- operators over regular expression
- Algebraics data types

### Changed

- Coments now begin with --, like haskell
- glue operator now is ` or ´ 

## [0.1.2] - 2017-10-29

### Added

- Reify
- notes on english and spanish 
- exception handling

### Changed

- type replaced by class and record

## [0.1.1] - 2017-03-01

### Changed

- Add vars
- Fix issues with grammar on comp-expr
- add more text to OGU-0.1 notes.

## [0.1.0] - 2017-02-26

### Changed

- First compiler and interpreter based on Clojure runtime


[Unreleased]: https://github.com/your-name/ogu-lang/compare/0.1.1...HEAD
[0.1.2]: https://github.com/your-name/ogu-lang/compare/0.1.1...0.1.2
[0.1.1]: https://github.com/your-name/ogu-lang/compare/0.1.0...0.1.1
