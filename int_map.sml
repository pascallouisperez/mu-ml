structure IntMap = RedBlackMapFn(
  struct
    type ord_key = int
    val compare = Int.compare
  end)
