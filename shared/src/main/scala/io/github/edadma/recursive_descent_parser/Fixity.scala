package io.github.edadma.recursive_descent_parser

enum Fixity:
  case Xfx  // infix, non-associative
  case Xfy  // infix, right-associative
  case Yfx  // infix, left-associative
  case Fx   // prefix, non-associative
  case Fy   // prefix, associative
  case Xf   // postfix, non-associative
  case Yf   // postfix, associative

  def isInfix: Boolean = this match
    case Xfx | Xfy | Yfx => true
    case _               => false

  def isPrefix: Boolean = this match
    case Fx | Fy => true
    case _       => false

  def isPostfix: Boolean = this match
    case Xf | Yf => true
    case _       => false

  // For infix/postfix: can left arg have same precedence?
  def leftY: Boolean = this match
    case Yfx | Yf => true
    case _        => false

  // For infix/prefix: can right arg have same precedence?
  def rightY: Boolean = this match
    case Xfy | Fy => true
    case _        => false
