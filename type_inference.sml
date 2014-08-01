signature
UNIFIER = sig
  val reset: unit -> unit
  val next_id: unit -> int
  val get: int * Ast.Type -> Ast.Type
  val put: int * Ast.Type -> unit
  val add: Ast.Type -> int
  val listItems: unit -> (int * Ast.Type) list
  val unify: Ast.Type * Ast.Type -> Ast.Type option
end

functor UnifierFn() :> UNIFIER =
struct

  val id = ref 0

  val canonicalized: (Ast.Type IntMap.map) ref = ref IntMap.empty

  fun reset() = (id := 0; canonicalized := IntMap.empty)

  fun next_id() = (id := !id + 1; !id)

  fun get(id: int, default: Ast.Type): Ast.Type =
    case IntMap.find(!canonicalized, id) of SOME(c) => c | NONE => default

  fun put(id: int, value: Ast.Type): unit =
    canonicalized := IntMap.insert(!canonicalized, id, value)

  fun add(value: Ast.Type): int =
    let
      val id = next_id()
    in
      IntMap.insert(!canonicalized, id, value);
      id
    end

  fun listItems() = IntMap.listItemsi (!canonicalized)

  fun unify(left: Ast.Type, right: Ast.Type): Ast.Type option = case (left, right) of
      (Ast.BaseType kl, Ast.BaseType kr) => if kl = kr then SOME(left) else NONE
    | (Ast.ArrowType(l1, l2), Ast.ArrowType(r1, r2)) => (
        case (unify(l1, r1), unify(l2, r2)) of
          (SOME(u1), SOME(u2)) => SOME(Ast.ArrowType(u1, u2))
        | _ => NONE)
    | (Ast.TypeVariable l, _) => (
      case IntMap.find(!canonicalized, l) of
        SOME(left') => unify(left', right)
      | NONE => (put(l, right); SOME(right)))
    | (_, Ast.TypeVariable _) => unify(right, left)
    | _ => NONE
    ;

end

structure
TypeInference =  struct

  structure Unifier = UnifierFn()

  fun infer(t: Ast.Exp): Ast.Type option = let
    fun op_of_type("+") = Ast.BaseType Ast.KInt
      | op_of_type("-") = Ast.BaseType Ast.KInt
      | op_of_type("*") = Ast.BaseType Ast.KInt
      | op_of_type("%") = Ast.BaseType Ast.KInt
      | op_of_type("/") = Ast.BaseType Ast.KInt
      | op_of_type("^") = Ast.BaseType Ast.KString
      ;

    fun inferImpl(exp: Ast.Exp, constraint_id: int): Ast.Type option =
      let
        val constraint_type = Unifier.get(constraint_id, Ast.TypeVariable constraint_id)
      in
        case exp of
          Ast.IntConstant _ => Unifier.unify(constraint_type, Ast.BaseType Ast.KInt)
        | Ast.StringConstant _ => Unifier.unify(constraint_type, Ast.BaseType Ast.KString)
        | Ast.Unit => Unifier.unify(constraint_type, Ast.BaseType Ast.KUnit)
        | Ast.InfixApp(l, opr, r) =>
            let
              val branch_type = op_of_type opr
              val branch_id = Unifier.add(branch_type)
            in
              case (inferImpl(l, branch_id), inferImpl(r, branch_id)) of
                (SOME(_), SOME(_)) => SOME(branch_type)
              | _ => NONE
            end
        | _ => NONE
      end
  in
    Unifier.reset();
    inferImpl(t, Unifier.next_id())
  end

end
