structure
SymbolAnalysis = struct

  fun resolve(t) = let
    val symbol_id = ref 0

    fun next_symbol(name) =
      {lab = name, id = (symbol_id := !symbol_id + 1; !symbol_id)}

    fun resolveImpl(s, Ast.Variable(r)) =
        let
          val name = #lab (!r)
        in
          case LinkedScope.find(s, name)
          of SOME(sym) => r := sym
          | NONE => r := next_symbol name
        end
      (*| resolveImpl(s, LetIn(decls, exp)) =>*)
      | resolveImpl(s, Ast.Fn(args, body)) =
        let
          val fnScope = LinkedScope.create_inner s
          val _ = List.map (fn(Ast.Name(r)) => let
              val name = #lab (!r)
              val symbol = next_symbol(name)
            in
              r := symbol;
              LinkedScope.insert(fnScope, name, symbol)
            end) args
        in
          resolveImpl(fnScope, body)
        end
      | resolveImpl(s, Ast.App(l, r)) = (resolveImpl(s, l); resolveImpl(s, r))
      | resolveImpl(s, Ast.InfixApp(l, _, r)) = (resolveImpl(s, l); resolveImpl(s, r))
      | resolveImpl(s, Ast.IfThenElse(c, l, r)) = (resolveImpl(s, c); resolveImpl(s, l); resolveImpl(s, r))
      | resolveImpl(s, Ast.IntConstant(_)) = ()
      | resolveImpl(s, Ast.StringConstant(_)) = ()
      ;
  in
    resolveImpl(LinkedScope.create_empty(), t)
  end

end
