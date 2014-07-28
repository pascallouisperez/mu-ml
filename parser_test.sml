let
  fun makeTest(input: string, expected: Ast.Exp) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_ast(input)
      in
        actuals = expected
      end));
in
  ConsoleTestRunner.runTestCase([
    makeTest("420", Ast.IntConstant(420))
  ])
end
