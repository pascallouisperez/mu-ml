let
  fun s(name) = ref {lab = name, id = 0};

  fun argeq(Ast.Name(l), Ast.Name(r)) = #lab (!l) = #lab (!r)
    ;

  fun listeq(comp)(l, r) = case (l, r)
    of (nil, nil) => true
    | (l1 :: l2, r1 :: r2) => comp(l1, r1) andalso listeq comp (l2, r2)
    | (_, _) => false
    ;

  fun eq(Ast.IntConstant(l1), Ast.IntConstant(r1)) = l1 = r1
    | eq(Ast.StringConstant(l1), Ast.StringConstant(r1)) = l1 = r1
    | eq(Ast.Variable(l1), Ast.Variable(r1)) = #lab (!l1) = #lab (!r1)
    | eq(Ast.App(l1, l2), Ast.App(r1, r2)) =
        eq(l1, r1) andalso
        eq(l2, r2)
    | eq(Ast.InfixApp(l1, opl, l2), Ast.InfixApp(r1, opr, r2)) =
        eq(l1, r1) andalso
        opl = opr andalso
        eq(l2, r2)
    | eq(Ast.IfThenElse(l1, l2, l3), Ast.IfThenElse(r1, r2, r3)) =
        eq(l1, r1) andalso
        eq(l2, r2) andalso
        eq(l3, r3)
    | eq(Ast.LetIn(l1, l2), Ast.LetIn(r1, r2)) =
        listeq eq (l1, r1) andalso
        eq(l2, r2)
    | eq(Ast.Fn(l1, l2), Ast.Fn(r1, r2)) =
        listeq argeq (l1, r1) andalso
        eq(l2, r2)
    | eq(Ast.Valdec(l1, l2), Ast.Valdec(r1, r2)) =
        argeq(l1, r1) andalso
        eq(l2, r2)
    | eq(_, _) = false
    ;

  fun makeTest(input: string, expected: Ast.Exp) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_ast(input)
      in
        eq(actuals, expected)
      end));
in
  ConsoleTestRunner.runTestCase([
    makeTest("420",
      Ast.IntConstant(420)
    ),
    makeTest("x",
      Ast.Variable(s("x"))
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
        Ast.Variable(s("x")),
        Ast.Variable(s("y"))
      )
    ),
    makeTest("x (y z)",
      Ast.App(
        Ast.Variable(s("x")),
        Ast.App(
          Ast.Variable(s("y")),
          Ast.Variable(s("z"))
        )
      )
    ),
    makeTest("x y z",
      Ast.App(
        Ast.Variable(s("x")),
        Ast.App(
          Ast.Variable(s("y")),
          Ast.Variable(s("z"))
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
            Ast.Name(s("x")),
            Ast.IntConstant(4)
          )
        ],
        Ast.Variable(s("x"))
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
        [Ast.Name(s("x"))],
        Ast.Variable(s("x"))
      )
    ),
    makeTest("fn(x) => x",
      Ast.Fn(
        [Ast.Name(s("x"))],
        Ast.Variable(s("x"))
      )
    ),
    makeTest("fn(x, y) => x",
      Ast.Fn(
        [Ast.Name(s("x")), Ast.Name(s("y"))],
        Ast.Variable(s("x"))
      )
    ),
    makeTest("(fn(x) => x) 6",
      Ast.App(
        Ast.Fn(
          [Ast.Name(s("x"))],
          Ast.Variable(s("x"))
        ),
        Ast.IntConstant(6)
      )
    )
  ])
end
