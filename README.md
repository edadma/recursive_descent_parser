# recursive_descent_parser

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/recursive_descent_parser_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/recursive_descent_parser)](https://github.com/edadma/recursive_descent_parser/commits)
![GitHub](https://img.shields.io/github/license/edadma/recursive_descent_parser)
![Scala Version](https://img.shields.io/badge/Scala-3.8.1-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A table-driven precedence climbing parser for Scala 3, designed for parsing Prolog and other languages with configurable operator precedence.

## Features

- **Configurable operator precedence and associativity** - all Prolog fixity types (xfx, xfy, yfx, fx, fy, xf, yf)
- **Configurable bracket syntax** - lists, curly braces, or custom delimiters with optional tail separators
- **Modular lexer extensions** - composable number format readers
- **Cross-platform** - JVM, JavaScript (Scala.js), and Native (Scala Native)

## Installation

```scala
libraryDependencies += "io.github.edadma" %%% "recursive_descent_parser" % "0.0.1"
```

## Quick Start

Use the included `PrologConfig` for parsing Prolog expressions:

```scala
import io.github.edadma.recursive_descent_parser.*

val lexer = PrologConfig.lexer
val parser = PrologConfig.parser

val result = parser.parse("foo(X, Y) :- bar(X), baz(Y)", lexer)
println(result)  // :-(foo(X,Y),,(bar(X),baz(Y)))
```

## Examples

### Arithmetic with Precedence

```scala
parser.parse("1 + 2 * 3", lexer)      // +(1,*(2,3))
parser.parse("(1 + 2) * 3", lexer)    // *(+(1,2),3)
```

### Lists

```scala
parser.parse("[1, 2, 3]", lexer)      // .(1,.(2,.(3,[])))
parser.parse("[H|T]", lexer)          // .(H,T)
parser.parse("[1, 2|Rest]", lexer)    // .(1,.(2,Rest))
```

### Operators

```scala
parser.parse("-X", lexer)             // -(X)       prefix
parser.parse("X = Y", lexer)          // =(X,Y)     infix
parser.parse("\\+ foo(X)", lexer)     // \+(foo(X)) prefix negation
parser.parse("a :- b, c", lexer)      // :-(a,,(b,c))
```

### Special Number Formats

```scala
parser.parse("0'a", lexer)            // 97         character code
parser.parse("0xFF", lexer)           // 255        hexadecimal
parser.parse("0b1010", lexer)         // 10         binary
parser.parse("0o77", lexer)           // 63         octal
parser.parse("1_000_000", lexer)      // 1000000    underscores
```

## Custom Configuration

Create your own parser configuration for different languages:

```scala
import io.github.edadma.recursive_descent_parser.*

// Define operators with precedence and fixity
val myOperators: Map[String, List[OpEntry]] = Map(
  "+"  -> List(OpEntry(500, Fixity.Yfx)),   // left-associative
  "*"  -> List(OpEntry(400, Fixity.Yfx)),   // higher precedence (tighter)
  "^"  -> List(OpEntry(200, Fixity.Xfy)),   // right-associative
  "-"  -> List(OpEntry(500, Fixity.Yfx), OpEntry(200, Fixity.Fy)), // infix and prefix
)

// Define bracket syntax
val myBrackets: List[BracketConfig] = List(
  BracketConfig("[", "]", ",", Some("|"), myListBuilder),
)

val myLexer = Lexer(
  symbols = myOperators.keySet ++ Set("(", ")", "[", "]", ",", "|"),
  numberReaders = List(HexReader, BinaryReader),  // pick what you need
)

val myParser = Parser(myOperators, myBrackets)
```

## Operator Fixity Types

| Fixity | Type | Associativity | Example |
|--------|------|---------------|---------|
| `Xfx` | infix | non-associative | `a = b` (but not `a = b = c`) |
| `Xfy` | infix | right-associative | `a ; b ; c` → `a ; (b ; c)` |
| `Yfx` | infix | left-associative | `a + b + c` → `(a + b) + c` |
| `Fx` | prefix | non-associative | `not x` |
| `Fy` | prefix | associative | `- - x` → `- (- x)` |
| `Xf` | postfix | non-associative | `x !` |
| `Yf` | postfix | associative | `x ! !` → `(x !) !` |

## Lexer Extensions

Modular number format readers can be composed as needed:

```scala
val lexer = Lexer(
  symbols = ...,
  numberReaders = List(
    CharCodeReader,  // 0'a -> 97
    HexReader,       // 0xFF -> 255
    BinaryReader,    // 0b1010 -> 10
    OctalReader,     // 0o77 -> 63
  ),
  allowUnderscoreInNumbers = true,  // 1_000_000
)
```

## AST

The parser produces `Term` values:

```scala
enum Term:
  case Atom(pos: CharReader, name: String)
  case Var(pos: CharReader, name: String)
  case Integer(pos: CharReader, value: Long)
  case Float(pos: CharReader, value: Double)
  case Str(pos: CharReader, value: String)
  case Compound(pos: CharReader, functor: String, args: List[Term])
```

Each term carries its source position for error reporting.

## License

ISC License - see [LICENSE](LICENSE) for details.
