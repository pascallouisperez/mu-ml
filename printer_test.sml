let
  fun s(name) = ref (Ast.create_symbol name)

  fun fromTo(input: string, expected: string) =
    (
      input,
      fn() => Assert.assertTrue(Ast.toString(Helpers.string_to_ast(input)) = expected)
    )
in
  ConsoleTestRunner.runTestCase([
    fromTo("1", "1"),
    fromTo("()", "()"),
    fromTo("fn(x) => x", "fn(x) => x"),
    fromTo("(fn(x) => x) 3", "(fn(x) => x) 3"),
    fromTo("foo(3)", "foo 3"),
    fromTo("fib(n-1) + fib(n-2)", "fib (n - 1) + fib (n - 2)"),
    fromTo("fn() => (x; y)", "fn() => (x; y)"),
    fromTo("fn() => if x then y else z", "fn() => if x then y else z"),
    fromTo("idx (if x then 1 else 2)", "idx (if x then 1 else 2)"),
    fromTo("let val x = (y; z) in 4 end", "let val x = (y; z) in 4 end"),
    fromTo("let val x = 3 in x end + 5", "let val x = 3 in x end + 5"),
    fromTo("let val idx = fn(x) => x in idx end 5", "let val idx = fn(x) => x in idx end 5"),
    fromTo("if (x; y) then (1; 1) else (2; 2)", "if (x; y) then (1; 1) else (2; 2)")
  ])
end
