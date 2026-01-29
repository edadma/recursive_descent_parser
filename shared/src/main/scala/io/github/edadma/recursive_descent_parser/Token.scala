package io.github.edadma.recursive_descent_parser

import io.github.edadma.char_reader.CharReader

enum Token:
  case AtomTok(pos: CharReader, name: String)
  case VarTok(pos: CharReader, name: String)
  case IntTok(pos: CharReader, value: Long)
  case FloatTok(pos: CharReader, value: Double)
  case StrTok(pos: CharReader, value: String)
  case SymTok(pos: CharReader, sym: String)
  case EOI(pos: CharReader)

  def pos: CharReader
