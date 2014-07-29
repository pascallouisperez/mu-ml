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
    makeTest("1+2+3",
      Ast.InfixApp(
        Ast.IntConstant(1),
        "+",
        Ast.InfixApp(
          Ast.IntConstant(2),
          "+",
          Ast.IntConstant(3)
        )
      )
    ),
    makeTest("x y",
      Ast.App(
        Ast.Variable("x"),
        Ast.Variable("y")
      )
    ),
    makeTest("x (y z)",
      Ast.App(
        Ast.Variable("x"),
        Ast.App(
          Ast.Variable("y"),
          Ast.Variable("z")
        )
      )
    ),
    makeTest("x y z",
      Ast.App(
        Ast.Variable("x"),
        Ast.App(
          Ast.Variable("y"),
          Ast.Variable("z")
        )
      )
    ),
    makeTest("if 1 then 2 else 3",
      Ast.IfThenElse(
        Ast.IntConstant(1),
        Ast.IntConstant(2),
        Ast.IntConstant(3)
        )
    ),
    makeTest("let val x = 4 in x end",
      Ast.LetIn(
        [
          Ast.Valdec(
            Ast.Name("x"),
            Ast.IntConstant(4)
          )
        ],
        Ast.Variable("x")
      )
    ),
    makeTest("fn() => 7",
      Ast.Fn(
        [],
        Ast.IntConstant(7)
      )
    ),
    makeTest("fn x => x",
      Ast.Fn(
        [Ast.Name("x")],
        Ast.Variable("x")
      )
    ),
    makeTest("fn(x) => x",
      Ast.Fn(
        [Ast.Name("x")],
        Ast.Variable("x")
      )
    ),
    makeTest("fn(x, y) => x",
      Ast.Fn(
        [Ast.Name("x"), Ast.Name("y")],
        Ast.Variable("x")
      )
    ),
    makeTest("(fn(x) => x) 6",
      Ast.App(
        Ast.Fn(
          [Ast.Name("x")],
          Ast.Variable("x")
        ),
        Ast.IntConstant(6)
      )
    )
  ])
end
