structure SymbolMap = RedBlackMapFn(
  struct
    type ord_key = Ast.Symbol
    val compare = fn(l: Ast.Symbol, r: Ast.Symbol) =>
      case String.compare(#lab l, #lab r)
      of EQUAL => Int.compare(#id l, #id r)
      | c => c
  end)
