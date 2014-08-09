let
  fun s(name) = ref (Ast.create_symbol name)

  fun makeTest(input: Ast.Exp, expected: Ast.Exp) = (Ast.toString(expected),
      fn() => Assert.assertTrue(let
        val _ = SymbolAnalysis.resolve(input)
      in
        Ast.eq(input, expected)
      end));
  fun verifyPolymorphic(input: string, expected: int list) = (input,
      fn() => Assert.assertTrue(let
        val actuals = #polymorphic_symbols (SymbolAnalysis.resolve (Helpers.string_to_ast input))
      in
        (IntBinarySet.listItems actuals) = expected
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
    makeTest(Helpers.string_to_ast("fn(x) => (x, y, z, x)"),
      Ast.Fn(
        [Ast.Name(ref {id=1,lab="x"})],
        Ast.Tuple([
          Ast.Variable(ref {lab="x",id=1}),
          Ast.Variable(ref {lab="y",id=2}),
          Ast.Variable(ref {lab="z",id=3}),
          Ast.Variable(ref {lab="x",id=1})
        ])
      )
    ),
    makeTest(Helpers.string_to_ast("fn(x, y) => (x; y; y; x; z)"),
      Ast.Fn(
        [Ast.Name(ref {id=1,lab="x"}), Ast.Name(ref {id=2,lab="y"})],
        Ast.Sequence([
          Ast.Variable(ref {lab="x",id=1}),
          Ast.Variable(ref {lab="y",id=2}),
          Ast.Variable(ref {lab="y",id=2}),
          Ast.Variable(ref {lab="x",id=1}),
          Ast.Variable(ref {lab="z",id=3})
        ])
      )
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
    ),
    makeTest(
      (* symbol x which is the body of the val def, is a new symbol (no recursive val defs) *)
      Ast.LetIn(
        [Ast.Valdec(Ast.Name(s("x")), false, Ast.Variable(s("x")))],
        Ast.Variable(s("x"))
      ),
      Ast.LetIn(
        [Ast.Valdec(Ast.Name(ref {id=1,lab="x"}), false, Ast.Variable(ref {id=2,lab="x"}))],
        Ast.Variable(ref {id=1,lab="x"})
      )
    ),
    makeTest(Helpers.string_to_ast("let val x = 42; val x = x; val x = x in x end"),
      Ast.LetIn(
        [
          Ast.Valdec(Ast.Name(ref {id=1,lab="x"}), false, Ast.IntConstant(42)),
          Ast.Valdec(Ast.Name(ref {id=2,lab="x"}), false, Ast.Variable(ref {id=1,lab="x"})),
          Ast.Valdec(Ast.Name(ref {id=3,lab="x"}), false, Ast.Variable(ref {id=2,lab="x"}))
        ],
        Ast.Variable(ref {id=3,lab="x"})
      )
    ),
    makeTest(
      (* symbol x which is the body of the fun def refers to the function being defined (recursion) *)
      Ast.LetIn(
        [Ast.Valdec(Ast.Name(s("x")), true, Ast.Fn([], Ast.Variable(s("x"))))],
        Ast.Variable(s("x"))
      ),
      Ast.LetIn(
        [Ast.Valdec(Ast.Name(ref {id=1,lab="x"}), true, Ast.Fn([], Ast.Variable(ref {id=1,lab="x"})))],
        Ast.Variable(ref {id=1,lab="x"})
      )
    ),
    makeTest(Helpers.string_to_ast("let fun a(x) = b(x); fun b(x) = x in b(9) end"),
      Ast.LetIn(
        [
          Ast.Valdec(
            Ast.Name(ref {id=1,lab="a"}),
            true,
            Ast.Fn(
              [Ast.Name(ref {id=2,lab="x"})],
              Ast.App(Ast.Variable(ref {id=3, lab="b"}), Ast.Variable(ref {id=2, lab="x"})))
          ),
          Ast.Valdec(
            Ast.Name(ref {id=4,lab="b"}),
            true,
            Ast.Fn(
              [Ast.Name(ref {id=5,lab="x"})],
              Ast.Variable(ref {id=5, lab="x"})
            )
          )
        ],
        Ast.App(Ast.Variable(ref {id=4, lab="b"}), Ast.IntConstant(9))
      )
    ),
    makeTest(Helpers.string_to_ast("let fun b(x) = x; fun a(x) = b(x) in b(9) end"),
      Ast.LetIn(
        [
          Ast.Valdec(
            Ast.Name(ref {id=1,lab="b"}),
            true,
            Ast.Fn(
              [Ast.Name(ref {id=2,lab="x"})],
              Ast.Variable(ref {id=2, lab="x"})
            )
          ),
          Ast.Valdec(
            Ast.Name(ref {id=3,lab="a"}),
            true,
            Ast.Fn(
              [Ast.Name(ref {id=4,lab="x"})],
              Ast.App(Ast.Variable(ref {id=1, lab="b"}), Ast.Variable(ref {id=4, lab="x"})))
          )
        ],
        Ast.App(Ast.Variable(ref {id=1, lab="b"}), Ast.IntConstant(9))
      )
    ),
    verifyPolymorphic("let fun a(b) = c in d end", [1]),
    verifyPolymorphic("fn(x) => let fun a(b) = c; val d = 4 in d end", [2,5])
  ])
end
