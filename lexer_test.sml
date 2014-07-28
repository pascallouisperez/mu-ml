let
  fun makeTest(input: string, expected: MumlTokens.token list) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_tokens(input)
      in
        actuals = expected
      end));
in
  ConsoleTestRunner.runTestCase([
    (* keywords *)
    makeTest("let", [MumlTokens.KW_let]),
    makeTest("in", [MumlTokens.KW_in])
  ])
end
