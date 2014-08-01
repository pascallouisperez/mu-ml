let
  fun checkUnifiy((left, right), optExpected) =
      (case optExpected of
          SOME(expected) => Ast.toString_type(left) ^ " U " ^ Ast.toString_type(right) ^ " = " ^ Ast.toString_type(expected)
        | NONE => Ast.toString_type(left) ^ " U " ^ Ast.toString_type(right) ^ " fails"
          ,
      fn() => Assert.assertTrue
        (
          TypeInference.Unifier.reset();
          TypeInference.Unifier.unify(left, right) = optExpected
        )
      )

  fun good(input: string, expected: Ast.Type) = (input ^ ": " ^ Ast.toString_type(expected),
      fn() => Assert.assertTrue(let
        val exp = Helpers.string_to_ast(input)
      in
        SymbolAnalysis.resolve(exp);
        case TypeInference.infer(exp)
        of SOME(t) => Ast.typeeq(expected, t)
        |  NONE => false
      end))

  fun bad(input: string) = ("BAD " ^ input,
      fn() => Assert.assertTrue(let
        val exp = Helpers.string_to_ast(input)
      in
        SymbolAnalysis.resolve(exp);
        case TypeInference.infer(exp)
        of SOME(t) => false
        |  NONE => true
      end))
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

    (* inference *)
    good("3", Ast.BaseType Ast.KInt),
    good("\"\"", Ast.BaseType Ast.KString),
    good("()", Ast.BaseType Ast.KUnit),
    good("3 + 8", Ast.BaseType Ast.KInt),
    bad("3 + ())")
  ])
end
