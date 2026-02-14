package io.github.edadma.recursive_descent_parser

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable

case class OpEntry(precedence: Int, fixity: Fixity)

case class BracketConfig(
    open: String,
    close: String,
    separator: String,
    tailSeparator: Option[String],
    build: (CharReader, List[Term], Option[Term]) => Term,
)

class Parser(
    operators: Map[String, List[OpEntry]],
    brackets: List[BracketConfig],
    maxPrecedence: Int = 1200,
):
  import Token.*
  import Term.*
  import Fixity.*

  private var tokens: LazyList[Token] = LazyList.empty

  private def current: Token = tokens.head
  private def pos: CharReader = current.pos

  private def advance(): Unit =
    tokens = tokens.tail

  private def check(sym: String): Boolean = current match
    case SymTok(_, s) if s == sym => true
    case _                        => false

  private def consume(sym: String): Unit =
    if !check(sym) then
      pos.error(s"expected '$sym'")
    advance()

  private def tryConsume(sym: String): Boolean =
    if check(sym) then
      advance()
      true
    else false

  private def isEOI: Boolean = current match
    case EOI(_) => true
    case _      => false

  // Peek at next token without consuming
  private def peekNext: Token = tokens.tail.head

  // Look up prefix operators for a given name
  private def prefixOp(name: String): Option[OpEntry] =
    operators.get(name).flatMap(_.find(e => e.fixity.isPrefix))

  // Look up infix operators for a given name
  private def infixOp(name: String): Option[OpEntry] =
    operators.get(name).flatMap(_.find(e => e.fixity.isInfix))

  // Look up postfix operators for a given name
  private def postfixOp(name: String): Option[OpEntry] =
    operators.get(name).flatMap(_.find(e => e.fixity.isPostfix))

  def parse(input: String, lexer: Lexer): Term =
    tokens = lexer.tokenize(input)
    val (result, _) = parseExpr(maxPrecedence)
    if !isEOI then
      pos.error("unexpected token after expression")
    result

  def parse(toks: LazyList[Token]): Term =
    tokens = toks
    val (result, _) = parseExpr(maxPrecedence)
    if !isEOI then
      pos.error("unexpected token after expression")
    result

  def parseExprPartial(
      toks: LazyList[Token],
      stopAt: Set[String] = Set.empty,
  ): (Term, LazyList[Token]) =
    tokens = toks
    val (result, _) = parseExpr(maxPrecedence, stopAt)
    (result, tokens)

  // Parse terms separated by '.' (for Prolog programs)
  def parseProgram(input: String, lexer: Lexer): List[Term] =
    tokens = lexer.tokenize(input)
    val terms = mutable.ListBuffer[Term]()
    while !isEOI do
      val (term, _) = parseExpr(maxPrecedence)
      terms += term
      if !isEOI then consume(".")
    terms.toList

  // Core precedence climbing algorithm
  private def parseExpr(maxPrec: Int, stopAt: Set[String] = Set.empty): (Term, Int) =
    var (left, leftPrec) = parsePrefixOrPrimary(maxPrec, stopAt)

    while !isEOI do
      val opName = current match
        case SymTok(_, s) if stopAt.contains(s)  => return (left, leftPrec)
        case AtomTok(_, s) if stopAt.contains(s) => return (left, leftPrec)
        case SymTok(_, s)  => s
        case AtomTok(_, s) => s
        case _             => return (left, leftPrec)

      // Try postfix first, then infix
      postfixOp(opName) match
        case Some(entry) if entry.precedence <= maxPrec =>
          val canApply = entry.fixity.leftY || leftPrec != entry.precedence
          if canApply then
            val opPos = pos
            advance()
            left = Compound(opPos, opName, List(left))
            leftPrec = entry.precedence
          else
            return (left, leftPrec)

        case _ =>
          infixOp(opName) match
            case Some(entry) if entry.precedence <= maxPrec =>
              val canApply = entry.fixity.leftY || leftPrec != entry.precedence
              if canApply then
                val opPos = pos
                advance()
                val rightMaxPrec =
                  if entry.fixity.rightY then entry.precedence
                  else entry.precedence - 1
                val (right, _) = parseExpr(rightMaxPrec, stopAt)
                left = Compound(opPos, opName, List(left, right))
                leftPrec = entry.precedence
              else
                return (left, leftPrec)
            case _ =>
              return (left, leftPrec)

    (left, leftPrec)

  // Returns (term, effectivePrecedence) — primaries and parenthesized exprs get 0,
  // prefix operator applications get the operator's precedence
  private def parsePrefixOrPrimary(maxPrec: Int, stopAt: Set[String]): (Term, Int) =
    current match
      case SymTok(opPos, opName) =>
        // Symbolic prefix operators are always treated as operators
        // Use quoted form '+'(a, b) to use as functor
        prefixOp(opName) match
          case Some(entry) if entry.precedence <= maxPrec =>
            advance()
            val argMaxPrec =
              if entry.fixity.rightY then entry.precedence
              else entry.precedence - 1
            val (arg, _) = parseExpr(argMaxPrec, stopAt)
            (Compound(opPos, opName, List(arg)), entry.precedence)
          case _ =>
            (parsePrimary(), 0)

      case AtomTok(opPos, opName) =>
        // Alphabetic operators (like 'not') followed by '(' are functors
        // This allows not(x) to be a compound term, not prefix 'not' applied to (x)
        prefixOp(opName) match
          case Some(entry) if entry.precedence <= maxPrec =>
            // Peek to see if followed by '('
            peekNext match
              case SymTok(_, "(") =>
                // Treat as functor, not prefix operator
                (parsePrimary(), 0)
              case _ =>
                advance()
                val argMaxPrec =
                  if entry.fixity.rightY then entry.precedence
                  else entry.precedence - 1
                val (arg, _) = parseExpr(argMaxPrec, stopAt)
                (Compound(opPos, opName, List(arg)), entry.precedence)
          case _ =>
            (parsePrimary(), 0)

      case _ =>
        (parsePrimary(), 0)

  private def parsePrimary(): Term =
    current match
      case IntTok(p, v) =>
        advance()
        Integer(p, v)

      case FloatTok(p, v) =>
        advance()
        Float(p, v)

      case StrTok(p, v) =>
        advance()
        Str(p, v)

      case VarTok(p, name) =>
        advance()
        Var(p, name)

      case AtomTok(p, name) =>
        advance()
        if check("(") then
          parseCompound(p, name)
        else
          Atom(p, name)

      case SymTok(p, sym) =>
        // Check for bracket
        brackets.find(_.open == sym) match
          case Some(config) =>
            advance()
            parseBracketedExpr(p, config)
          case None =>
            if sym == "(" then
              advance()
              val (inner, _) = parseExpr(maxPrecedence)
              consume(")")
              inner
            else
              // Treat symbol as atom (for operators used as atoms)
              advance()
              if check("(") then
                parseCompound(p, sym)
              else
                Atom(p, sym)

      case EOI(p) =>
        p.error("unexpected end of input")

  private def parseCompound(pos: CharReader, functor: String): Term =
    consume("(")
    if check(")") then
      advance()
      Compound(pos, functor, Nil)
    else
      val args = parseArgs()
      consume(")")
      Compound(pos, functor, args)

  private def parseArgs(): List[Term] =
    val args = mutable.ListBuffer[Term]()
    val (first, _) = parseExpr(999) // Below comma precedence
    args += first
    while tryConsume(",") do
      val (arg, _) = parseExpr(999)
      args += arg
    args.toList

  private def parseBracketedExpr(startPos: CharReader, config: BracketConfig): Term =
    if check(config.close) then
      advance()
      config.build(startPos, Nil, None)
    else
      val elements = mutable.ListBuffer[Term]()
      val (first, _) = parseExpr(999) // Below comma
      elements += first

      while tryConsume(config.separator) do
        val (elem, _) = parseExpr(999)
        elements += elem

      val tail = config.tailSeparator match
        case Some(sep) if tryConsume(sep) =>
          val (t, _) = parseExpr(999)
          Some(t)
        case _ => None

      consume(config.close)
      config.build(startPos, elements.toList, tail)
