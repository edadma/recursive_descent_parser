package io.github.edadma.recursive_descent_parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers:
  val lexer: Lexer = PrologConfig.lexer
  val parser: Parser = PrologConfig.parser

  def parse(input: String): String = parser.parse(input, lexer).toString

  def parseFloat(input: String): Double = parser.parse(input, lexer) match
    case Term.Float(_, v) => v
    case other            => fail(s"expected Float, got $other")

  "Atoms" - {
    "simple atom" in { parse("foo") shouldBe "foo" }
    "atom with digits" in { parse("foo123") shouldBe "foo123" }
    "single letter" in { parse("x") shouldBe "x" }
    "quoted atom" in { parse("'hello world'") shouldBe "hello world" }
    "quoted atom with escape" in { parse("'it\\'s'") shouldBe "it's" }
  }

  "Variables" - {
    "uppercase start" in { parse("X") shouldBe "X" }
    "underscore start" in { parse("_foo") shouldBe "_foo" }
    "anonymous" in { parse("_") shouldBe "_" }
    "mixed case" in { parse("MyVar123") shouldBe "MyVar123" }
  }

  "Numbers" - {
    "integer" in { parse("42") shouldBe "42" }
    "zero" in { parse("0") shouldBe "0" }
    "float" in { parse("3.14") shouldBe "3.14" }
    "float with exponent" in { parseFloat("1.5e10") shouldBe 1.5e10 }
    "integer exponent" in { parseFloat("2E5") shouldBe 200000.0 }
  }

  "Strings" - {
    "double quoted" in { parse("\"hello\"") shouldBe "\"hello\"" }
    "with escape" in { parse("\"line1\\nline2\"") shouldBe "\"line1\nline2\"" }
    "empty" in { parse("\"\"") shouldBe "\"\"" }
  }

  "Compound terms" - {
    "no args" in { parse("foo()") shouldBe "foo()" }
    "one arg" in { parse("foo(x)") shouldBe "foo(x)" }
    "two args" in { parse("foo(x, y)") shouldBe "foo(x,y)" }
    "nested" in { parse("foo(bar(x), y)") shouldBe "foo(bar(x),y)" }
    "with numbers" in { parse("point(1, 2, 3)") shouldBe "point(1,2,3)" }
    "with variables" in { parse("foo(X, Y)") shouldBe "foo(X,Y)" }
  }

  "Lists" - {
    "empty list" in { parse("[]") shouldBe "[]" }
    "single element" in { parse("[1]") shouldBe ".(1,[])" }
    "multiple elements" in { parse("[1, 2, 3]") shouldBe ".(1,.(2,.(3,[])))" }
    "head tail" in { parse("[H|T]") shouldBe ".(H,T)" }
    "head multiple tail" in { parse("[1, 2|T]") shouldBe ".(1,.(2,T))" }
    "nested lists" in { parse("[[1, 2], [3, 4]]") shouldBe ".(.(1,.(2,[])),.(.(3,.(4,[])),[]))" }
    "list of atoms" in { parse("[a, b, c]") shouldBe ".(a,.(b,.(c,[])))" }
  }

  "Curly braces" - {
    "single term" in { parse("{x}") shouldBe "{}(x)" }
    "multiple terms" in { parse("{a, b, c}") shouldBe "{}(,(a,,(b,c)))" }
    "empty" in { parse("{}") shouldBe "{}({})" }
  }

  "Arithmetic precedence" - {
    "add/multiply" in { parse("1 + 2 * 3") shouldBe "+(1,*(2,3))" }
    "multiply/add" in { parse("1 * 2 + 3") shouldBe "+(*(1,2),3)" }
    "subtract/divide" in { parse("6 - 4 / 2") shouldBe "-(6,/(4,2))" }
    "parentheses override" in { parse("(1 + 2) * 3") shouldBe "*(+(1,2),3)" }
    "nested parens" in { parse("((1 + 2))") shouldBe "+(1,2)" }
  }

  "Arithmetic associativity" - {
    "left assoc add" in { parse("1 + 2 + 3") shouldBe "+(+(1,2),3)" }
    "left assoc sub" in { parse("1 - 2 - 3") shouldBe "-(-(1,2),3)" }
    "left assoc mul" in { parse("2 * 3 * 4") shouldBe "*(*(2,3),4)" }
    "right assoc power" in { parse("2 ^ 3 ^ 4") shouldBe "^(2,^(3,4))" }
  }

  "Prefix operators" - {
    "unary minus" in { parse("-1") shouldBe "-(1)" }
    "unary minus var" in { parse("-X") shouldBe "-(X)" }
    "unary plus" in { parse("+X") shouldBe "+(X)" }
    "negation" in { parse("\\+ foo") shouldBe "\\+(foo)" }
    "double minus" in { parse("- -X") shouldBe "-(-(X))" }
    "minus in expr" in { parse("1 + -2") shouldBe "+(1,-(2))" }
    "bitwise not" in { parse("\\X") shouldBe "\\(X)" }
  }

  "Comparison operators" - {
    "equals" in { parse("X = Y") shouldBe "=(X,Y)" }
    "not equals" in { parse("X \\= Y") shouldBe "\\=(X,Y)" }
    "identical" in { parse("X == Y") shouldBe "==(X,Y)" }
    "not identical" in { parse("X \\== Y") shouldBe "\\==(X,Y)" }
    "less than" in { parse("X < Y") shouldBe "<(X,Y)" }
    "greater than" in { parse("X > Y") shouldBe ">(X,Y)" }
    "less equal" in { parse("X =< Y") shouldBe "=<(X,Y)" }
    "greater equal" in { parse("X >= Y") shouldBe ">=(X,Y)" }
    "arithmetic equals" in { parse("X =:= Y") shouldBe "=:=(X,Y)" }
    "arithmetic not equals" in { parse("X =\\= Y") shouldBe "=\\=(X,Y)" }
  }

  "Prolog operators" - {
    "is" in { parse("X is 1 + 2") shouldBe "is(X,+(1,2))" }
    "univ" in { parse("X =.. Y") shouldBe "=..(X,Y)" }
    "clause" in { parse("head :- body") shouldBe ":-(head,body)" }
    "query" in { parse("?- goal") shouldBe "?-(goal)" }
    "directive" in { parse(":- goal") shouldBe ":-(goal)" }
    "conjunction" in { parse("a, b, c") shouldBe ",(a,,(b,c))" }
    "disjunction" in { parse("a ; b ; c") shouldBe ";(a,;(b,c))" }
    "if-then" in { parse("a -> b") shouldBe "->(a,b)" }
    "if-then-else" in { parse("a -> b ; c") shouldBe ";(->(a,b),c)" }
  }

  "Complex expressions" - {
    "clause with body" in {
      parse("foo(X) :- bar(X), baz(X)") shouldBe ":-(foo(X),,(bar(X),baz(X)))"
    }
    "arithmetic expression" in {
      parse("X is (A + B) * C / D") shouldBe "is(X,/(*(+(A,B),C),D))"
    }
    "list processing" in {
      parse("append([H|T], L, [H|R]) :- append(T, L, R)") shouldBe
        ":-(append(.(H,T),L,.(H,R)),append(T,L,R))"
    }
    "nested compound" in {
      parse("f(g(h(x)))") shouldBe "f(g(h(x)))"
    }
    "mixed operators" in {
      parse("X > 0, Y is X - 1") shouldBe ",(>(X,0),is(Y,-(X,1)))"
    }
  }

  "Comments" - {
    "line comment" in { parse("foo % this is a comment\n") shouldBe "foo" }
    "block comment" in { parse("foo /* comment */ ") shouldBe "foo" }
    "comment in expression" in { parse("1 + /* middle */ 2") shouldBe "+(1,2)" }
  }

  "Whitespace" - {
    "spaces" in { parse("  foo  ") shouldBe "foo" }
    "newlines" in { parse("\nfoo\n") shouldBe "foo" }
    "tabs" in { parse("\tfoo\t") shouldBe "foo" }
    "mixed" in { parse("  \n\t  foo  \t\n  ") shouldBe "foo" }
  }

  "Edge cases" - {
    "operator as functor" in { parse("'+'(1, 2)") shouldBe "+(1,2)" }
    "parenthesized atom" in { parse("(foo)") shouldBe "foo" }
    "deeply nested" in { parse("((((x))))") shouldBe "x" }
    "list in compound" in { parse("foo([1, 2], X)") shouldBe "foo(.(1,.(2,[])),X)" }
    "compound in list" in { parse("[foo(1), bar(2)]") shouldBe ".(foo(1),.(bar(2),[]))" }
    "cut" in { parse("!") shouldBe "!" }
    "clause with cut" in { parse("a :- b, !, c") shouldBe ":-(a,,(b,,(!,c)))" }
    "empty list atom" in { parse("foo([], X)") shouldBe "foo([],X)" }
    "list with expressions" in { parse("[1+2, 3*4]") shouldBe ".(+(1,2),.(*(3,4),[]))" }
    "nested curly" in { parse("{{x}}") shouldBe "{}({}(x))" }
    "semicolon in parens" in { parse("(a ; b)") shouldBe ";(a,b)" }
    "arrow in parens" in { parse("(a -> b ; c)") shouldBe ";(->(a,b),c)" }
  }

  "Special atoms" - {
    "empty list as arg" in { parse("length([], 0)") shouldBe "length([],0)" }
    "dot functor no quotes needed" in { parse(".(a, b)") shouldBe ".(a,b)" }
    "dot functor with quotes" in { parse("'.'(a, b)") shouldBe ".(a,b)" }
    "comma is operator needs quotes" in { parse("','(a, b)") shouldBe ",(a,b)" }
    "plus is operator needs quotes" in { parse("'+'(1, 2)") shouldBe "+(1,2)" }
    "prefix plus with parens" in { parse("+(a + b)") shouldBe "+(+(a,b))" }
    "quoted empty" in { parse("''") shouldBe "" }
    "quoted with spaces" in { parse("'hello world'(X)") shouldBe "hello world(X)" }
    "dot with many args" in { parse(".(a, b, c, [])") shouldBe ".(a,b,c,[])" }
    "dot as explicit cons" in { parse(".(1, .(2, []))") shouldBe ".(1,.(2,[]))" }
  }

  "Operator edge cases" - {
    "prefix then infix" in { parse("-1 + 2") shouldBe "+(-(1),2)" }
    "double prefix" in { parse("- - 1") shouldBe "-(-(1))" }
    "prefix in compound" in { parse("foo(-X, +Y)") shouldBe "foo(-(X),+(Y))" }
    "low prec in high context" in { parse("X = a, b") shouldBe ",(=(X,a),b)" }
    "comparison chain" in { parse("1 < 2, 2 < 3") shouldBe ",(<(1,2),<(2,3))" }
  }

  "Real Prolog patterns" - {
    "member/2" in {
      parse("member(X, [X|_])") shouldBe "member(X,.(X,_))"
    }
    "member/2 recursive" in {
      parse("member(X, [_|T]) :- member(X, T)") shouldBe
        ":-(member(X,.(_,T)),member(X,T))"
    }
    "length/2 base" in {
      parse("length([], 0)") shouldBe "length([],0)"
    }
    "length/2 recursive" in {
      parse("length([_|T], N) :- length(T, N1), N is N1 + 1") shouldBe
        ":-(length(.(_,T),N),,(length(T,N1),is(N,+(N1,1))))"
    }
    "factorial" in {
      parse("fact(0, 1) :- !") shouldBe ":-(fact(0,1),!)"
    }
    "factorial recursive" in {
      parse("fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1") shouldBe
        ":-(fact(N,F),,(>(N,0),,(is(N1,-(N,1)),,(fact(N1,F1),is(F,*(N,F1))))))"
    }
    "DCG rule" in {
      parse("sentence --> noun_phrase, verb_phrase") shouldBe
        "-->(sentence,,(noun_phrase,verb_phrase))"
    }
    "if-then-else full" in {
      parse("(X > 0 -> Y = positive ; Y = non_positive)") shouldBe
        ";(->(>(X,0),=(Y,positive)),=(Y,non_positive))"
    }
    "negation as failure" in {
      parse("\\+ member(X, L)") shouldBe "\\+(member(X,L))"
    }
    "assert" in {
      parse("assert(foo(1))") shouldBe "assert(foo(1))"
    }
    "clause with multiple goals" in {
      parse("foo(X, Y, Z) :- a(X), b(Y), c(Z), d(X, Y, Z)") shouldBe
        ":-(foo(X,Y,Z),,(a(X),,(b(Y),,(c(Z),d(X,Y,Z)))))"
    }
  }

  "Potential problems" - {
    "minus vs negative" in {
      // -3 is prefix minus applied to 3, not a negative literal
      parse("-3") shouldBe "-(3)"
    }
    "minus in list" in {
      parse("[-1, -2]") shouldBe ".(-(1),.(-(2),[]))"
    }
    "plus minus" in {
      parse("1 + -2") shouldBe "+(1,-(2))"
    }
    "multiply negative" in {
      parse("3 * -2") shouldBe "*(3,-(2))"
    }
    "power non-assoc single" in {
      parse("2 ** 3") shouldBe "**(2,3)"
    }
    "caret right assoc" in {
      // ^ is xfy so it chains right-associatively
      parse("2 ^ 3 ^ 4") shouldBe "^(2,^(3,4))"
    }
    "mod operator" in {
      parse("10 mod 3") shouldBe "mod(10,3)"
    }
    "rem operator" in {
      parse("10 rem 3") shouldBe "rem(10,3)"
    }
    "integer division" in {
      parse("10 // 3") shouldBe "//(10,3)"
    }
    "bitwise and" in {
      parse("5 /\\ 3") shouldBe "/\\(5,3)"
    }
    "bitwise or" in {
      parse("5 \\/ 3") shouldBe "\\/(5,3)"
    }
    "shift left" in {
      parse("1 << 4") shouldBe "<<(1,4)"
    }
    "shift right" in {
      parse("16 >> 2") shouldBe ">>(16,2)"
    }
    "term ordering" in {
      parse("X @< Y") shouldBe "@<(X,Y)"
    }
    "colon operator" in {
      parse("module:predicate") shouldBe ":(module,predicate)"
    }
  }

  "Non-associativity (xfx)" - {
    // xfx operators like = and ** cannot be chained
    "equals cannot chain" in {
      // a = b = c should fail because = is xfx
      assertThrows[RuntimeException] {
        parse("a = b = c")
      }
    }
    "power cannot chain" in {
      // 2 ** 3 ** 4 should fail because ** is xfx
      assertThrows[RuntimeException] {
        parse("2 ** 3 ** 4")
      }
    }
    "comparison cannot chain" in {
      // 1 < 2 < 3 should fail because < is xfx
      assertThrows[RuntimeException] {
        parse("1 < 2 < 3")
      }
    }
    "mixed xfx ok with different ops" in {
      // But different operators at same precedence should work
      parse("X = Y, A = B") shouldBe ",(=(X,Y),=(A,B))"
    }
    "parenthesized expr as operand of xfx at same op-table precedence" in {
      // + has a 200/fy entry in the table; ** is 200/xfx
      // (1 + 2) is parenthesized so its effective precedence is 0,
      // but hasSamePrecedence looks up "+" and finds the 200 entry
      parse("(1 + 2) ** 3") shouldBe "**(+(1,2),3)"
    }
    "parenthesized xfx as operand of same-precedence xfx" in {
      // = and < are both 700/xfx; without parens this correctly fails,
      // but (a = b) has effective precedence 0 so it should work as left of <
      parse("(a = b) < c") shouldBe "<(=(a,b),c)"
    }
  }

  "Program parsing" - {
    def parseProgram(input: String): List[String] =
      parser.parseProgram(input, lexer).map(_.toString)

    "single fact" in {
      parseProgram("foo.") shouldBe List("foo")
    }
    "multiple facts" in {
      parseProgram("foo. bar. baz.") shouldBe List("foo", "bar", "baz")
    }
    "fact and rule" in {
      parseProgram("base(0). rec(N) :- N > 0.") shouldBe
        List("base(0)", ":-(rec(N),>(N,0))")
    }
    "with whitespace and comments" in {
      parseProgram("""
        % This is a comment
        foo(1).
        /* block comment */
        bar(2).
      """) shouldBe List("foo(1)", "bar(2)")
    }
  }

  "Special number formats" - {
    "character code lowercase" in { parse("0'a") shouldBe "97" }
    "character code uppercase" in { parse("0'A") shouldBe "65" }
    "character code space" in { parse("0' ") shouldBe "32" }
    "character code newline escape" in { parse("0'\\n") shouldBe "10" }
    "character code tab escape" in { parse("0'\\t") shouldBe "9" }
    "character code backslash" in { parse("0'\\\\") shouldBe "92" }
    "hex lowercase" in { parse("0xff") shouldBe "255" }
    "hex uppercase" in { parse("0XFF") shouldBe "255" }
    "hex mixed" in { parse("0xDeAdBeEf") shouldBe "3735928559" }
    "binary" in { parse("0b1010") shouldBe "10" }
    "binary uppercase" in { parse("0B1111") shouldBe "15" }
    "octal" in { parse("0o77") shouldBe "63" }
    "octal uppercase" in { parse("0O777") shouldBe "511" }
    "underscore in integer" in { parse("1_000_000") shouldBe "1000000" }
    "underscore in parts" in { parse("123_456_789") shouldBe "123456789" }
  }

  "Number formats in expressions" - {
    "char code in arithmetic" in { parse("0'a + 1") shouldBe "+(97,1)" }
    "hex in comparison" in { parse("X =:= 0xFF") shouldBe "=:=(X,255)" }
    "binary in list" in { parse("[0b1, 0b10, 0b11]") shouldBe ".(1,.(2,.(3,[])))" }
    "underscore number in compound" in { parse("big(1_000_000)") shouldBe "big(1000000)" }
  }

  "parseExprPartial with stop-set" - {
    def partial(input: String, stopAt: Set[String]): (String, List[Token]) =
      val toks = lexer.tokenize(input)
      val (term, rest) = parser.parseExprPartial(toks, stopAt)
      (term.toString, rest.toList)

    "stop at keyword (atom token)" in {
      val (term, rest) = partial("1 + 2 then 3", Set("then"))
      term shouldBe "+(1,2)"
      rest.head shouldBe a[Token.AtomTok]
      rest.head.asInstanceOf[Token.AtomTok].name shouldBe "then"
    }
    "stop at symbol operator" in {
      val (term, rest) = partial("a + b; c", Set(";"))
      term shouldBe "+(a,b)"
      rest.head shouldBe a[Token.SymTok]
      rest.head.asInstanceOf[Token.SymTok].sym shouldBe ";"
    }
    "prefix operator propagates stop-set" in {
      val (term, rest) = partial("- x then y", Set("then"))
      term shouldBe "-(x)"
      rest.head.asInstanceOf[Token.AtomTok].name shouldBe "then"
    }
    "no stop token parses to EOI" in {
      val (term, rest) = partial("1 + 2", Set("then"))
      term shouldBe "+(1,2)"
      rest.head shouldBe a[Token.EOI]
    }
    "stop token inside parens is ignored" in {
      val (term, rest) = partial("f(a, b) then c", Set("then"))
      term shouldBe "f(a,b)"
      rest.head.asInstanceOf[Token.AtomTok].name shouldBe "then"
    }
    "stop token inside brackets is ignored" in {
      val (term, rest) = partial("[a, b] then c", Set("then"))
      term shouldBe ".(a,.(b,[]))"
      rest.head.asInstanceOf[Token.AtomTok].name shouldBe "then"
    }
  }
