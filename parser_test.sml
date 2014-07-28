let
  fun makeTest(input: string, expected: Ast.Exp) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_ast(input)
      in
        actuals = expected
      end));
in
  ConsoleTestRunner.runTestCase([
    makeTest("420",
      Ast.IntConstant(420)
    ),
    makeTest("x",
      Ast.Variable("x")
    ),
    makeTest("1+2",
      Ast.InfixApp(
        Ast.IntConstant(1),
        "+",
        Ast.IntConstant(2)
      )
    ),
    makeTest("if 1 then 2 else 3",
      Ast.IfThenElse(
        Ast.IntConstant(1),
        Ast.IntConstant(2),
        Ast.IntConstant(3)
        )
    )
  ])
end
