package io.github.edadma.recursive_descent_parser

@main def run(): Unit =
  val lexer = PrologConfig.lexer
  val parser = PrologConfig.parser

  val examples = List(
    "1 + 2 * 3",
    "(1 + 2) * 3",
    "foo(X, Y)",
    "[1, 2, 3]",
    "[H|T]",
    "a :- b, c",
    "X is 1 + 2",
    "-X",
    "\\+ foo(X)",
    "'+'(1, 2)",
    ".(a, b)",
    "+(a + b)",
  )

  for expr <- examples do
    println(s"Input: $expr")
    try
      val result = parser.parse(expr, lexer)
      println(s"  AST: $result")
    catch
      case e: Exception => println(s"  Error: ${e.getMessage}")
    println()
