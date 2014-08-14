let
  fun checkUnifiy((left, right), optExpected) =
      (case optExpected of
          SOME(expected) => Ast.toString_type(left) ^ " U " ^ Ast.toString_type(right) ^ " = " ^ Ast.toString_type(expected)
        | NONE => Ast.toString_type(left) ^ " U " ^ Ast.toString_type(right) ^ " fails"
          ,
      fn() => Assert.assertTrue
        (
          TypeInference.Unifier.reset(0);
          TypeInference.Unifier.unify(left, right) = optExpected
        )
      )

  fun good(input: string, expected: Ast.Type) = (input ^ ": " ^ Ast.toString_type(expected),
      fn() => Assert.assertTrue(
        case Helpers.string_to_infer(input) of
          SOME(t) => expected = t
        | NONE => false
      )
    )

  fun bad(input: string) = ("BAD " ^ input,
      fn() => Assert.assertTrue(
        case Helpers.string_to_infer(input) of
          SOME(t) => false
        | NONE => true
      )
    )
in
  ConsoleTestRunner.runTestCase([
    (* unification *)
    checkUnifiy(
      (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt),
      SOME(Ast.BaseType Ast.KInt)
    ),
    checkUnifiy(
      (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KString),
      NONE
    ),
    checkUnifiy(
      (Ast.TypeVariable 1, Ast.BaseType Ast.KString),
      SOME(Ast.BaseType Ast.KString)
    ),
    checkUnifiy(
      (Ast.BaseType Ast.KString, Ast.TypeVariable 1),
      SOME(Ast.BaseType Ast.KString)
    ),
    checkUnifiy(
      (Ast.ArrowType(Ast.TypeVariable 1, Ast.TypeVariable 1), Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)),
      SOME(Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt))
    ),
    checkUnifiy(
      (Ast.ArrowType(Ast.TypeVariable 1, Ast.TypeVariable 1), Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KString)),
      NONE
    ),
    checkUnifiy(
      (Ast.ArrowType(Ast.TypeVariable 1, Ast.TypeVariable 2), Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KString)),
      SOME(Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KString))
    ),
    checkUnifiy(
      (Ast.TypeVariable 1, Ast.ArrowType(Ast.TypeVariable 2, Ast.TypeVariable 2)),
      SOME(Ast.ArrowType(Ast.TypeVariable 2, Ast.TypeVariable 2))
    ),
    checkUnifiy(
      (Ast.TypeVariable 1, Ast.TypeVariable 2),
      SOME(Ast.TypeVariable 1)
    ),
    checkUnifiy(
      (Ast.TypeVariable 2, Ast.TypeVariable 1),
      SOME(Ast.TypeVariable 1)
    ),
    (
      "1 U 2, 3 U 4, 2 U 4, all to 1",
      fn() => Assert.assertTrue let
      in
        TypeInference.Unifier.reset(0);
        TypeInference.Unifier.unify(Ast.TypeVariable 1, Ast.TypeVariable 2);
        TypeInference.Unifier.unify(Ast.TypeVariable 3, Ast.TypeVariable 4);
        TypeInference.Unifier.unify(Ast.TypeVariable 2, Ast.TypeVariable 3);

        [Ast.TypeVariable 1, Ast.TypeVariable 1, Ast.TypeVariable 1, Ast.TypeVariable 1]
          =
        List.map (fn(i) => TypeInference.Unifier.get i) [1, 2, 3, 4]
      end
    ),

    (* inference *)
    good("3", Ast.BaseType Ast.KInt),
    good("\"\"", Ast.BaseType Ast.KString),
    good("()", Ast.BaseType Ast.KUnit),
    good("3 + 8", Ast.BaseType Ast.KInt),
    bad("3 + ()"),
    good("(1,2,\"three\")", Ast.TupleType([Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt, Ast.BaseType Ast.KString])),
    good("fn() => 1", Ast.ArrowType(Ast.BaseType Ast.KUnit, Ast.BaseType Ast.KInt)),
    good("fn(x) => x", Ast.ArrowType(Ast.TypeVariable 1, Ast.TypeVariable 1)),
    good("fn(x) => x + x", Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)),
    good("fn(x) => fn(y) => x + y", Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.ArrowType(Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt))),
    good("(fn(x) => x) 3", Ast.BaseType Ast.KInt),
    good("\"1\"; (); 3", Ast.BaseType Ast.KInt),
    bad("() ^ (); 8"),
    good("let val x = () in x end", Ast.BaseType Ast.KUnit),
    good("let fun id(x) = x in id 3 end", Ast.BaseType Ast.KInt),
    good("let fun id(x) = x in id 3; id () end", Ast.BaseType Ast.KUnit),
    good("if 4 = 5 then () else ()", Ast.BaseType Ast.KUnit),
    bad("if 1 then () else ()")
  ])
end
