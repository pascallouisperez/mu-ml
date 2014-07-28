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
    makeTest("(),;~", [MumlTokens.LP, MumlTokens.RP, MumlTokens.COMMA, MumlTokens.SEMI, MumlTokens.NEG]),
    makeTest("=", [MumlTokens.EQ]),
    makeTest("=>", [MumlTokens.ARROW]),
    makeTest("==>", [MumlTokens.EQ, MumlTokens.ARROW]),

    (* keywords *)
    makeTest("if", [MumlTokens.KW_if]),
    makeTest("then", [MumlTokens.KW_then]),
    makeTest("else", [MumlTokens.KW_else]),
    makeTest("andalso", [MumlTokens.KW_andalso]),
    makeTest("orelse", [MumlTokens.KW_orelse]),

    makeTest("fn", [MumlTokens.KW_fn]),
    makeTest("fun", [MumlTokens.KW_fun]),

    makeTest("let", [MumlTokens.KW_let]),
    makeTest("in", [MumlTokens.KW_in]),
    makeTest("end", [MumlTokens.KW_end])
  ])
end
