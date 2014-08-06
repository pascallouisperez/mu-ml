structure
SymbolAnalysis = struct

  type result = {program: Ast.Exp, max_symbol_id: int}

  fun resolve(t) = let
    val symbol_id = ref 0

    fun next_symbol(name: string) =
      {lab = name, id = (symbol_id := !symbol_id + 1; !symbol_id)}

    fun resolveArgImpl(s, Ast.Name(r)) =
      let
        val name = #lab (!r)
        val symbol = next_symbol(name)
      in
        r := symbol;
        LinkedScope.insert(s, name, symbol);
        (name, symbol)
      end

    fun resolveImpl(s, Ast.Variable(r)) =
        let
          val name = #lab (!r)
        in
          case LinkedScope.find(s, name)
          of SOME(sym) => (r := sym; s)
          | NONE => (r := next_symbol name; s)
        end
      | resolveImpl(s, Ast.Fn(args, body)) =
        let
          val fnScope = LinkedScope.create_inner s
        in
          List.map (fn(arg) => resolveArgImpl(fnScope, arg)) args;
          resolveImpl(fnScope, body);
          s
        end
      | resolveImpl(s, Ast.LetIn(decls, body)) =
          let
            val bodyScope = List.foldl (fn(e, s) => resolveImpl(s, e)) s decls
          in
            resolveImpl(bodyScope, body); s
          end
      | resolveImpl(s, Ast.Valdec(arg, body)) =
          let
            val bodyScope = LinkedScope.create_inner s
            val valScope = LinkedScope.create_inner s
            val (name, symbol) = (resolveImpl(bodyScope, body); resolveArgImpl(valScope, arg))
          in
            LinkedScope.insert(valScope, name, symbol);
            valScope
          end
      | resolveImpl(s, Ast.Fundec(arg, body)) =
          let
            val bodyScope = LinkedScope.create_inner s
            val valScope = LinkedScope.create_inner s
            val (name, symbol) = resolveArgImpl(valScope, arg)
          in
            LinkedScope.insert(valScope, name, symbol);
            LinkedScope.insert(bodyScope, name, symbol);
            resolveImpl(bodyScope, body);
            valScope
          end
      | resolveImpl(s, Ast.App(l, r)) = (resolveImpl(s, l); resolveImpl(s, r); s)
      | resolveImpl(s, Ast.InfixApp(l, _, r)) = (resolveImpl(s, l); resolveImpl(s, r); s)
      | resolveImpl(s, Ast.IfThenElse(c, l, r)) = (resolveImpl(s, c); resolveImpl(s, l); resolveImpl(s, r); s)
      | resolveImpl(s, Ast.Tuple(l)) = (List.map (fn(t) => resolveImpl(s, t)) l; s)
      | resolveImpl(s, Ast.IntConstant(_)) = s
      | resolveImpl(s, Ast.StringConstant(_)) = s
      | resolveImpl(s, Ast.Unit) = s
      ;
  in
    resolveImpl(LinkedScope.create_empty(), t);
    {program = t, max_symbol_id = !symbol_id}
  end

end
