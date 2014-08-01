structure StringMap = RedBlackMapFn(
  struct
    type ord_key = string
    val compare = String.compare
  end)

structure SymbolMap = RedBlackMapFn(
  struct
    type ord_key = Ast.Symbol
    val compare = fn(l: Ast.Symbol, r: Ast.Symbol) =>
      case String.compare(#lab l, #lab r)
      of EQUAL => Int.compare(#id l, #id r)
      | c => c
  end)

structure IntMap = RedBlackMapFn(
  struct
    type ord_key = int
    val compare = Int.compare
  end)
