let
  fun s(name) = ref (Ast.create_symbol name)

  fun fromTo(input: string, expected: string) =
    (
      input,
      fn() => Assert.assertTrue(Ast.toString(Helpers.string_to_ast(input)) = expected)
    )

  fun fromToSame(input) = fromTo(input, input)
in
  ConsoleTestRunner.runTestCase([
    fromToSame("1"),
    fromToSame("()"),
    fromToSame("(1, two, \"three\")"),
    fromToSame("fn(x) => x"),
    fromToSame("(fn(x) => x) 3"),
    fromTo("foo(3)", "foo 3"),
    fromToSame("(a b) (c d)"),
    fromToSame("#1 (1; (1, 2))"),
    fromToSame("(#1 f) (#2 f)"),
    fromTo("fib(n-1) + fib(n-2)", "fib (n - 1) + fib (n - 2)"),
    fromToSame("fn() => (x; y)"),
    fromToSame("fn() => if x then y else z"),
    fromToSame("idx (if x then 1 else 2)"),
    fromToSame("if 3 + 4 then y else z"),
    fromToSame("let val x = (y; z) in 4 end"),
    fromToSame("let val x = 3 in x end + 5"),
    fromToSame("let val idx = fn(x) => x in idx end 5"),
    fromToSame("if (x; y) then (1; 1) else (2; 2)")
  ])
end
