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

    (* keywords *)
    makeTest("let", [MumlTokens.KW_let]),
    makeTest("in", [MumlTokens.KW_in])
  ])
end
