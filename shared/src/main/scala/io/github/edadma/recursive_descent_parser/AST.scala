package io.github.edadma.recursive_descent_parser

import io.github.edadma.char_reader.CharReader

enum Term:
  case Atom(pos: CharReader, name: String)
  case Var(pos: CharReader, name: String)
  case Integer(pos: CharReader, value: Long)
  case Float(pos: CharReader, value: Double)
  case Str(pos: CharReader, value: String)
  case Compound(pos: CharReader, functor: String, args: List[Term])

  def pos: CharReader

  override def toString: String = this match
    case Atom(_, name)         => name
    case Var(_, name)          => name
    case Integer(_, value)     => value.toString
    case Float(_, value)       => value.toString
    case Str(_, value)         => s"\"$value\""
    case Compound(_, f, args)  => s"$f(${args.mkString(",")})"
