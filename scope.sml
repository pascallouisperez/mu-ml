structure
LinkedScope = struct

  datatype scope = S of Ast.Symbol StringMap.map ref * scope option

  fun create_empty() = S(ref StringMap.empty, NONE)

  fun create_inner(outer) = S(ref StringMap.empty, SOME(outer))

  fun insert(S(m, optNext), name, symbol) =
    m := StringMap.insert(!m, name, symbol)

  fun find(S(m, optNext), name) = case (StringMap.find(!m, name), optNext)
    of (SOME(sym), _) => SOME(sym)
    |  (NONE, SOME(next)) => find(next, name)
    |  (_, _) => NONE
    ;

end
