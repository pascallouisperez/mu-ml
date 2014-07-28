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

    (* punctuation *)
    makeTest("()", [MumlTokens.LP, MumlTokens.RP]),
    makeTest("[]", [MumlTokens.LB, MumlTokens.RB]),
    makeTest(",;~:", [MumlTokens.COMMA, MumlTokens.SEMI, MumlTokens.NEG, MumlTokens.COLON]),
    makeTest("=", [MumlTokens.EQ]),
    makeTest("=>", [MumlTokens.ARROW]),
    makeTest("==>", [MumlTokens.EQ, MumlTokens.ARROW]),
    makeTest("->", [MumlTokens.TARROW]),
    makeTest("*", [MumlTokens.TSTAR]),

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
      MumlTokens.EQ, MumlTokens.ID("x")
    ]),
    makeTest("val identity = fn(x) => x", [
      MumlTokens.KW_val, MumlTokens.ID("identity"), MumlTokens.EQ, MumlTokens.KW_fn, MumlTokens.LP,
      MumlTokens.ID("x"), MumlTokens.RP, MumlTokens.ARROW, MumlTokens.ID("x")
    ])
  ])
end
