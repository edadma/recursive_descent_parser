package io.github.edadma.recursive_descent_parser

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class Lexer(
    symbols: Set[String],
    lineComment: Option[String] = Some("%"),
    blockComment: Option[(String, String)] = Some(("/*", "*/")),
):
  import Token.*

  // Sort symbols by length descending for longest-match
  private val sortedSymbols = symbols.toList.sortBy(-_.length)

  def tokenize(input: String): LazyList[Token] =
    tokenize(CharReader.fromString(input))

  def tokenize(r: CharReader): LazyList[Token] =
    val skipped = skipWhitespaceAndComments(r)
    if skipped.eoi then
      EOI(skipped) #:: LazyList.empty
    else
      val (tok, next) = readToken(skipped)
      tok #:: tokenize(next)

  @tailrec
  private def skipWhitespaceAndComments(r: CharReader): CharReader =
    if r.eoi then r
    else if r.ch.isWhitespace then skipWhitespaceAndComments(r.next)
    else if lineComment.exists(lc => r.matches(lc).isDefined) then
      skipWhitespaceAndComments(skipToEol(r))
    else if blockComment.exists((s, _) => r.matches(s).isDefined) then
      blockComment match
        case Some((start, end)) =>
          r.matches(start) match
            case Some(after) => skipWhitespaceAndComments(skipBlockComment(after, end))
            case None        => r
        case None => r
    else r

  @tailrec
  private def skipToEol(r: CharReader): CharReader =
    if r.eoi || r.ch == '\n' then r
    else skipToEol(r.next)

  @tailrec
  private def skipBlockComment(r: CharReader, end: String): CharReader =
    if r.eoi then r.error(s"unclosed block comment")
    else r.matches(end) match
      case Some(after) => after
      case None        => skipBlockComment(r.next, end)

  private def readToken(r: CharReader): (Token, CharReader) =
    val ch = r.ch

    if ch == '_' || ch.isLetter then
      readIdentOrVar(r)
    else if ch.isDigit then
      readNumber(r)
    else if ch == '"' then
      readString(r, '"')
    else if ch == '\'' then
      readQuotedAtom(r)
    else
      readSymbol(r)

  private def readIdentOrVar(r: CharReader): (Token, CharReader) =
    val start = r
    val (name, rest) = consumeWhile(r, c => c == '_' || c.isLetterOrDigit)
    val tok =
      if name.head == '_' || name.head.isUpper then VarTok(start, name)
      else AtomTok(start, name)
    (tok, rest)

  private def readNumber(r: CharReader): (Token, CharReader) =
    val start = r
    val (intPart, afterInt) = consumeWhile(r, _.isDigit)

    if afterInt.ch == '.' && afterInt.next.ch.isDigit then
      val (fracPart, afterFrac) = consumeWhile(afterInt.next, _.isDigit)
      val (expPart, afterExp) = readExponent(afterFrac)
      val value = s"$intPart.$fracPart$expPart".toDouble
      (FloatTok(start, value), afterExp)
    else if afterInt.ch == 'e' || afterInt.ch == 'E' then
      val (expPart, afterExp) = readExponent(afterInt)
      val value = s"$intPart$expPart".toDouble
      (FloatTok(start, value), afterExp)
    else
      (IntTok(start, intPart.toLong), afterInt)

  private def readExponent(r: CharReader): (String, CharReader) =
    if r.ch == 'e' || r.ch == 'E' then
      val buf = new StringBuilder
      buf += r.ch
      var cur = r.next
      if cur.ch == '+' || cur.ch == '-' then
        buf += cur.ch
        cur = cur.next
      if !cur.ch.isDigit then
        r.error("expected exponent digits")
      val (digits, rest) = consumeWhile(cur, _.isDigit)
      (buf.toString + digits, rest)
    else
      ("", r)

  private def readString(r: CharReader, delim: Char): (Token, CharReader) =
    val start = r
    val buf = new StringBuilder
    var cur = r.next // skip opening delimiter

    while !cur.eoi && cur.ch != delim do
      if cur.ch == '\\' then
        cur = cur.next
        if cur.eoi then cur.error("unexpected end of input in string escape")
        buf += escapeChar(cur.ch)
        cur = cur.next
      else
        buf += cur.ch
        cur = cur.next

    if cur.eoi then start.error(s"unclosed string literal")
    (StrTok(start, buf.toString), cur.next) // skip closing delimiter

  private def readQuotedAtom(r: CharReader): (Token, CharReader) =
    val start = r
    val buf = new StringBuilder
    var cur = r.next // skip opening quote

    while !cur.eoi && cur.ch != '\'' do
      if cur.ch == '\\' then
        cur = cur.next
        if cur.eoi then cur.error("unexpected end of input in quoted atom escape")
        buf += escapeChar(cur.ch)
        cur = cur.next
      else
        buf += cur.ch
        cur = cur.next

    if cur.eoi then start.error("unclosed quoted atom")
    (AtomTok(start, buf.toString), cur.next) // skip closing quote

  private def escapeChar(c: Char): Char = c match
    case 'n'  => '\n'
    case 't'  => '\t'
    case 'r'  => '\r'
    case '\\' => '\\'
    case '"'  => '"'
    case '\'' => '\''
    case '0'  => '\u0000'
    case other => other

  private def readSymbol(r: CharReader): (Token, CharReader) =
    // Try to match longest symbol first
    sortedSymbols.iterator
      .flatMap(sym => r.matches(sym).map(after => (SymTok(r, sym), after)))
      .nextOption()
      .getOrElse((SymTok(r, r.ch.toString), r.next))

  @tailrec
  private def consumeWhile(r: CharReader, pred: Char => Boolean, acc: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi || !pred(r.ch) then (acc.toString, r)
    else
      acc += r.ch
      consumeWhile(r.next, pred, acc)
