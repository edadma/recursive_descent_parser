package io.github.edadma.recursive_descent_parser

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec

/** Lexer extension for reading special number formats.
  * Each extension tries to read a number starting at the given position.
  * Returns Some((token, nextReader)) if successful, None otherwise.
  */
trait NumberReader:
  def read(r: CharReader): Option[(Token, CharReader)]

/** Reads character codes: 0'a -> 97, 0'\n -> 10, etc. */
object CharCodeReader extends NumberReader:
  def read(r: CharReader): Option[(Token, CharReader)] =
    if r.ch == '0' && r.more then
      val next = r.next
      if next.ch == '\'' && next.more then
        val charPos = next.next
        if charPos.eoi then None
        else if charPos.ch == '\\' && charPos.more then
          // Escape sequence
          val escPos = charPos.next
          val code = escapeCode(escPos.ch)
          Some((Token.IntTok(r, code.toLong), escPos.next))
        else
          // Regular character
          Some((Token.IntTok(r, charPos.ch.toLong), charPos.next))
      else None
    else None

  private def escapeCode(c: Char): Int = c match
    case 'n'  => '\n'.toInt
    case 't'  => '\t'.toInt
    case 'r'  => '\r'.toInt
    case '\\' => '\\'.toInt
    case '\'' => '\''.toInt
    case '0'  => 0
    case c    => c.toInt

/** Reads hexadecimal numbers: 0xFF, 0XFF -> 255 */
object HexReader extends NumberReader:
  def read(r: CharReader): Option[(Token, CharReader)] =
    if r.ch == '0' && r.more then
      val next = r.next
      if (next.ch == 'x' || next.ch == 'X') && next.more then
        val start = next.next
        if !isHexDigit(start.ch) then
          r.error("expected hex digit after 0x")
        val (digits, rest) = consumeHex(start)
        Some((Token.IntTok(r, java.lang.Long.parseLong(digits, 16)), rest))
      else None
    else None

  private def isHexDigit(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  @tailrec
  private def consumeHex(r: CharReader, acc: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi || !isHexDigit(r.ch) then (acc.toString, r)
    else
      acc += r.ch
      consumeHex(r.next, acc)

/** Reads binary numbers: 0b1010, 0B1010 -> 10 */
object BinaryReader extends NumberReader:
  def read(r: CharReader): Option[(Token, CharReader)] =
    if r.ch == '0' && r.more then
      val next = r.next
      if (next.ch == 'b' || next.ch == 'B') && next.more then
        val start = next.next
        if !isBinaryDigit(start.ch) then
          r.error("expected binary digit after 0b")
        val (digits, rest) = consumeBinary(start)
        Some((Token.IntTok(r, java.lang.Long.parseLong(digits, 2)), rest))
      else None
    else None

  private def isBinaryDigit(c: Char): Boolean = c == '0' || c == '1'

  @tailrec
  private def consumeBinary(r: CharReader, acc: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi || !isBinaryDigit(r.ch) then (acc.toString, r)
    else
      acc += r.ch
      consumeBinary(r.next, acc)

/** Reads octal numbers: 0o77, 0O77 -> 63 */
object OctalReader extends NumberReader:
  def read(r: CharReader): Option[(Token, CharReader)] =
    if r.ch == '0' && r.more then
      val next = r.next
      if (next.ch == 'o' || next.ch == 'O') && next.more then
        val start = next.next
        if !isOctalDigit(start.ch) then
          r.error("expected octal digit after 0o")
        val (digits, rest) = consumeOctal(start)
        Some((Token.IntTok(r, java.lang.Long.parseLong(digits, 8)), rest))
      else None
    else None

  private def isOctalDigit(c: Char): Boolean = c >= '0' && c <= '7'

  @tailrec
  private def consumeOctal(r: CharReader, acc: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi || !isOctalDigit(r.ch) then (acc.toString, r)
    else
      acc += r.ch
      consumeOctal(r.next, acc)

/** Wrapper that allows underscores in numbers: 1_000_000 -> 1000000 */
object UnderscoreNumberReader:
  def wrapDigitConsumer(consume: (CharReader, Char => Boolean) => (String, CharReader)): (CharReader, Char => Boolean) => (String, CharReader) =
    (r, pred) =>
      val (digits, rest) = consumeWithUnderscores(r, pred)
      (digits.filterNot(_ == '_'), rest)

  @tailrec
  private def consumeWithUnderscores(r: CharReader, pred: Char => Boolean, acc: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi then (acc.toString, r)
    else if pred(r.ch) then
      acc += r.ch
      consumeWithUnderscores(r.next, pred, acc)
    else if r.ch == '_' && r.more && pred(r.next.ch) then
      acc += '_'
      consumeWithUnderscores(r.next, pred, acc)
    else
      (acc.toString, r)
