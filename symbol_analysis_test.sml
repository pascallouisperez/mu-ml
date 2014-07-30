let
  fun s(name) = ref (Ast.create_symbol name)

  fun makeTest(input: Ast.Exp, expected: Ast.Exp) = (Ast.toString(expected),
      fn() => Assert.assertTrue(let
        val _ = SymbolAnalysis.resolve(input)
      in
        Ast.eq(input, expected)
      end));
in
  ConsoleTestRunner.runTestCase([
    makeTest(
      Ast.Fn([Ast.Name(s("x"))], Ast.Variable(s("x"))),
      Ast.Fn([Ast.Name(ref {id=1,lab="x"})], Ast.Variable(ref {lab="x",id=1}))
    ),
    makeTest(
      Ast.Fn([Ast.Name(s("x"))], Ast.Variable(s("y"))),
      Ast.Fn([Ast.Name(ref {id=1,lab="x"})], Ast.Variable(ref {lab="y",id=2}))
    ),
    makeTest(
      Ast.Fn(
        [Ast.Name(s("x")), Ast.Name(s("y"))],
        Ast.InfixApp(Ast.Variable(s("x")), "+", Ast.Variable(s("y")))
      ),
      Ast.Fn(
        [Ast.Name(ref {id=1,lab="x"}), Ast.Name(ref {id=2,lab="y"})],
        Ast.InfixApp(
          Ast.Variable(ref {lab="x",id=1}),
          "+",
          Ast.Variable(ref {lab="y",id=2})
        )
      )
    ),
    makeTest(
      Ast.Fn([Ast.Name(s("x"))], Ast.App(Ast.Variable(s("x")), Ast.Variable(s("x")))),
      Ast.Fn(
        [Ast.Name(ref {id=1,lab="x"})],
        Ast.App(
          Ast.Variable(ref {id=1,lab="x"}),
          Ast.Variable(ref {id=1,lab="x"})
        )
      )
    ),
    makeTest(
      Ast.Fn([Ast.Name(s("x"))], Ast.IfThenElse(Ast.Variable(s("x")), Ast.Variable(s("x")), Ast.Variable(s("x")))),
      Ast.Fn(
        [Ast.Name(ref {id=1,lab="x"})],
        Ast.IfThenElse(
          Ast.Variable(ref {id=1,lab="x"}),
          Ast.Variable(ref {id=1,lab="x"}),
          Ast.Variable(ref {id=1,lab="x"})
        )
      )
    )
  ])
end
