package io.github.edadma.recursive_descent_parser

import io.github.edadma.char_reader.CharReader

object PrologConfig:
  import Fixity.*
  import Term.*

  // Standard ISO Prolog operators
  val operators: Map[String, List[OpEntry]] = Map(
    ":-"   -> List(OpEntry(1200, Xfx), OpEntry(1200, Fx)),
    "-->"  -> List(OpEntry(1200, Xfx)),
    "?-"   -> List(OpEntry(1200, Fx)),
    ";"    -> List(OpEntry(1100, Xfy)),
    "|"    -> List(OpEntry(1100, Xfy)),  // Also used in lists, but can be operator
    "->"   -> List(OpEntry(1050, Xfy)),
    ","    -> List(OpEntry(1000, Xfy)),
    "\\+"  -> List(OpEntry(900, Fy)),
    "="    -> List(OpEntry(700, Xfx)),
    "\\="  -> List(OpEntry(700, Xfx)),
    "=="   -> List(OpEntry(700, Xfx)),
    "\\==" -> List(OpEntry(700, Xfx)),
    "@<"   -> List(OpEntry(700, Xfx)),
    "@>"   -> List(OpEntry(700, Xfx)),
    "@=<"  -> List(OpEntry(700, Xfx)),
    "@>="  -> List(OpEntry(700, Xfx)),
    "=.."  -> List(OpEntry(700, Xfx)),
    "is"   -> List(OpEntry(700, Xfx)),
    "=:="  -> List(OpEntry(700, Xfx)),
    "=\\=" -> List(OpEntry(700, Xfx)),
    "<"    -> List(OpEntry(700, Xfx)),
    ">"    -> List(OpEntry(700, Xfx)),
    "=<"   -> List(OpEntry(700, Xfx)),
    ">="   -> List(OpEntry(700, Xfx)),
    ":"    -> List(OpEntry(600, Xfy)),
    "+"    -> List(OpEntry(500, Yfx), OpEntry(200, Fy)),
    "-"    -> List(OpEntry(500, Yfx), OpEntry(200, Fy)),
    "/\\"  -> List(OpEntry(500, Yfx)),
    "\\/"  -> List(OpEntry(500, Yfx)),
    "*"    -> List(OpEntry(400, Yfx)),
    "/"    -> List(OpEntry(400, Yfx)),
    "//"   -> List(OpEntry(400, Yfx)),
    "rem"  -> List(OpEntry(400, Yfx)),
    "mod"  -> List(OpEntry(400, Yfx)),
    "<<"   -> List(OpEntry(400, Yfx)),
    ">>"   -> List(OpEntry(400, Yfx)),
    "**"   -> List(OpEntry(200, Xfx)),
    "^"    -> List(OpEntry(200, Xfy)),
    "\\"   -> List(OpEntry(200, Fy)),
  )

  // All symbols that can be operators
  val symbols: Set[String] = operators.keySet ++ Set(
    "(", ")", "[", "]", "{", "}", ".", ",", "|",
  )

  // Prolog list syntax: [a, b, c] and [H|T]
  def buildList(pos: CharReader, elements: List[Term], tail: Option[Term]): Term =
    elements.foldRight(tail.getOrElse(Atom(pos, "[]"))): (elem, acc) =>
      Compound(pos, ".", List(elem, acc))

  // Prolog curly braces: {a, b} -> '{}'(','(a, b))
  def buildCurly(pos: CharReader, elements: List[Term], tail: Option[Term]): Term =
    val inner = elements match
      case Nil      => Atom(pos, "{}")
      case e :: Nil => e
      case es       => es.reduceRight((a, b) => Compound(pos, ",", List(a, b)))
    Compound(pos, "{}", List(inner))

  val brackets: List[BracketConfig] = List(
    BracketConfig("[", "]", ",", Some("|"), buildList),
    BracketConfig("{", "}", ",", None, buildCurly),
  )

  def lexer: Lexer = Lexer(symbols)

  def parser: Parser = Parser(operators, brackets)
