let
  fun makeTest(input: string, expected: MumlTokens.token list) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_tokens(input)
      in
        actuals = expected
      end));
in
  ConsoleTestRunner.runTestCase([
    (* constants *)
    makeTest("0", [MumlTokens.CON_int(0)]),
    makeTest("99", [MumlTokens.CON_int(99)]),

    makeTest("\"\"", [MumlTokens.CON_string("")]),
    makeTest("\"a\"", [MumlTokens.CON_string("a")]),
    makeTest("\"hello world\"", [MumlTokens.CON_string("hello world")]),
    makeTest("\"\\t\"", [MumlTokens.CON_string("\t")]),

    (* punctuation *)
    makeTest("()", [MumlTokens.LP, MumlTokens.RP]),
    makeTest("[]", [MumlTokens.LB, MumlTokens.RB]),
    makeTest(",;:", [MumlTokens.COMMA, MumlTokens.SEMI, MumlTokens.COLON]),
    makeTest("=>", [MumlTokens.ARROW]),
    makeTest("->", [MumlTokens.TARROW]),

    (* operators *)
    makeTest("=", [MumlTokens.OP("=")]),
    makeTest("*", [MumlTokens.OP("*")]),
    makeTest(">", [MumlTokens.OP(">")]),
    makeTest("<", [MumlTokens.OP("<")]),
    makeTest("+", [MumlTokens.OP("+")]),
    makeTest("-", [MumlTokens.OP("-")]),
    makeTest("^", [MumlTokens.OP("^")]),
    makeTest("==>", [MumlTokens.OP("==>")]),
    makeTest("><+-", [MumlTokens.OP("><+-")]),

    (* keywords *)
    makeTest("if", [MumlTokens.KW_if]),
    makeTest("then", [MumlTokens.KW_then]),
    makeTest("else", [MumlTokens.KW_else]),
    makeTest("andalso", [MumlTokens.KW_andalso]),
    makeTest("orelse", [MumlTokens.KW_orelse]),

    makeTest("fn", [MumlTokens.KW_fn]),
    makeTest("fun", [MumlTokens.KW_fun]),
    makeTest("val", [MumlTokens.KW_val]),

    makeTest("let", [MumlTokens.KW_let]),
    makeTest("in", [MumlTokens.KW_in]),
    makeTest("end", [MumlTokens.KW_end]),

    (* identifiers *)
    makeTest("a a1 a' a_", [MumlTokens.ID("a"), MumlTokens.ID("a1"), MumlTokens.ID("a'"), MumlTokens.ID("a_")]),
    makeTest("ab_ab_ab FooBar'Baz", [MumlTokens.ID("ab_ab_ab"), MumlTokens.ID("FooBar'Baz")]),

    (* programs *)
    makeTest("fun identity(x) = x", [
      MumlTokens.KW_fun, MumlTokens.ID("identity"), MumlTokens.LP, MumlTokens.ID("x"), MumlTokens.RP,
      MumlTokens.OP("="), MumlTokens.ID("x")
    ]),
    makeTest("val identity = fn(x) => x", [
      MumlTokens.KW_val, MumlTokens.ID("identity"), MumlTokens.OP("="), MumlTokens.KW_fn, MumlTokens.LP,
      MumlTokens.ID("x"), MumlTokens.RP, MumlTokens.ARROW, MumlTokens.ID("x")
    ]),
    makeTest("fun fib(n) = if n > 2 then fib(n-1) + fib(n-2) else 1", [
      MumlTokens.KW_fun, MumlTokens.ID("fib"), MumlTokens.LP, MumlTokens.ID("n"), MumlTokens.RP,
      MumlTokens.OP("="), MumlTokens.KW_if, MumlTokens.ID("n"), MumlTokens.OP(">"), MumlTokens.CON_int(2),
      MumlTokens.KW_then, MumlTokens.ID("fib"), MumlTokens.LP, MumlTokens.ID("n"), MumlTokens.OP("-"),
      MumlTokens.CON_int(1), MumlTokens.RP, MumlTokens.OP("+"), MumlTokens.ID("fib"), MumlTokens.LP,
      MumlTokens.ID("n"), MumlTokens.OP("-"), MumlTokens.CON_int(2), MumlTokens.RP,
      MumlTokens.KW_else, MumlTokens.CON_int(1)
    ])
  ])
end
