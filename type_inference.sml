signature
UNIFIER = sig
  val reset: int -> unit
  val next_id: unit -> int
  val get: int -> Ast.Type
  val put: int * Ast.Type -> unit
  val add: Ast.Type -> int
  val listItems: unit -> (int * Ast.Type) list
  val unify: Ast.Type * Ast.Type -> Ast.Type option
end

functor UnifierFn() :> UNIFIER =
struct

  val id = ref 0

  val canonicalized: (Ast.Type IntMap.map) ref = ref IntMap.empty

  fun reset(new_id) = (id := new_id; canonicalized := IntMap.empty)

  fun next_id() = (id := !id + 1; !id)

  fun get(id: int): Ast.Type =
    case IntMap.find(!canonicalized, id) of
      SOME(Ast.TypeVariable id') => if id = id' then Ast.TypeVariable id' else get id'
    | SOME(t) => t
    | NONE => Ast.TypeVariable id

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

  fun unify(left: Ast.Type, right: Ast.Type): Ast.Type option =
    if left = right then SOME(left)
    else case (left, right) of
      (Ast.BaseType kl, Ast.BaseType kr) => if kl = kr then SOME(left) else NONE
    | (Ast.ArrowType(l1, l2), Ast.ArrowType(r1, r2)) => (
        case (unify(l1, r1), unify(l2, r2)) of
          (SOME(u1), SOME(u2)) => SOME(Ast.ArrowType(u1, u2))
        | _ => NONE)
    | (Ast.TypeVariable l, _) => (
      case IntMap.find(!canonicalized, l) of
        SOME(left') => unify(left', right)
      | NONE => (
        case right of
          Ast.TypeVariable r =>
            if l < r
            then (put(r, left); SOME(left))
            else (put(l, right); SOME(right))
        | _ => (put(l, right); SOME(right))
        )
      )
    | (_, Ast.TypeVariable _) => unify(right, left)
    | _ => NONE
    ;

end

functor DebugUnifierFn(delegate: UNIFIER) :> UNIFIER =
struct

  fun reset(id) = (
    print ("reset " ^ (Int.toString id) ^ "\n");
    delegate.reset(id)
    )

  fun next_id() = (
    print "next_id()\n";
    delegate.next_id()
    )

  fun get(id) = (
    print ("get " ^ (Int.toString id) ^ "\n");
    delegate.get(id)
    )

  fun put(id, value) = (
    print ("put " ^ (Int.toString id) ^ ", " ^ (Ast.toString_type value) ^ "\n");
    delegate.put(id, value)
    )

  fun add(value) = (
    print ("add " ^ (Ast.toString_type value) ^ "\n");
    delegate.add(value)
    )

  fun listItems() = delegate.listItems()

  fun unify(left, right) = (
    print ("unify " ^ (Ast.toString_type left) ^ ", " ^ (Ast.toString_type right) ^ "\n");
    delegate.unify(left, right)
    )

end

structure
TypeInference =  struct

  structure Unifier = UnifierFn()

  fun infer(t: SymbolAnalysis.result): Ast.Type option = let
    fun 'a apply(f: 'a -> Ast.Type option)(opt: 'a option): Ast.Type option =
      case opt of SOME t => f t | NONE => NONE

    fun argType(Ast.Name r) = Unifier.get (#id (!r))

    fun fresh(t) =
      let
        val assigns: (int IntMap.map) ref = ref IntMap.empty
        fun freshImpl(t) = case t of
            Ast.BaseType k => t
          | Ast.TypeVariable r => (case IntMap.find(!assigns, r) of
              SOME r' => Ast.TypeVariable r'
            | NONE =>
              let
                val r' = Unifier.next_id()
              in
                assigns := IntMap.insert(!assigns, r, r');
                Ast.TypeVariable r'
              end
            )
          | Ast.ArrowType(l, r) => Ast.ArrowType(freshImpl l, freshImpl r)
          | Ast.TupleType(ts) => Ast.TupleType(List.map freshImpl ts)
      in
        freshImpl t
      end

    fun inferImpl(exp: Ast.Exp, constraint_id: int): Ast.Type option =
      let
        fun multiInferImpl(subExps: Ast.Exp list): Ast.Type list option =
          List.foldr
            (fn(optType, optList) => case (optType, optList) of
                (SOME t, SOME l) => SOME(t :: l)
              | _ => NONE)
            (SOME [])
            (List.map (fn(subExp) => inferImpl(subExp, Unifier.next_id())) subExps)

        val optInferedType = case exp of
            Ast.IntConstant _ => SOME(Ast.BaseType Ast.KInt)
          | Ast.StringConstant _ => SOME(Ast.BaseType Ast.KString)
          | Ast.Unit => SOME(Ast.BaseType Ast.KUnit)
          | Ast.InfixApp(l, opr, r) =>
              let
                val (branchType, oprResultType) = case opr of
                    "+" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)
                  | "-" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)
                  | "*" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)
                  | "%" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)
                  | "/" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KInt)
                  | "^" => (Ast.BaseType Ast.KString, Ast.BaseType Ast.KString)
                  | "=" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KBool)
                  | "<" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KBool)
                  | ">" => (Ast.BaseType Ast.KInt, Ast.BaseType Ast.KBool)
                val branchId = Unifier.add(branchType)
              in
                case (inferImpl(l, branchId), inferImpl(r, branchId)) of
                  (SOME lType, SOME rType) => (
                    case (Unifier.unify(branchType, lType), Unifier.unify(branchType, rType)) of
                      (SOME _, SOME _) => SOME(oprResultType)
                    | _ => NONE
                    )
                | _ => NONE
              end
          | Ast.App(fnExp, argExp) =>
            let
              val (fnId, argId) = (Unifier.next_id(), Unifier.next_id())
            in
              case (inferImpl(fnExp, fnId), inferImpl(argExp, argId)) of
                (SOME fnType, SOME argType) =>
                  apply
                    (fn(_) => SOME(Ast.TypeVariable constraint_id))
                    (Unifier.unify(fnType, Ast.ArrowType(argType, Ast.TypeVariable constraint_id)))
              | _ => NONE
            end
          | Ast.Tuple subExps =>
            apply (fn(ts) => SOME(Ast.TupleType ts)) (multiInferImpl subExps)
          | Ast.Sequence subExps =>
            apply (fn(ts) => Unifier.unify(Ast.TypeVariable constraint_id, List.hd (List.rev ts))) (multiInferImpl subExps)
          | Ast.Fn(args, body) =>
            let
              val bodyTypeOpt = inferImpl(body, Unifier.next_id())

              val argsType =
                case args of
                  nil => Ast.BaseType Ast.KUnit
                | hd :: nil => argType hd
                | hd :: tl => Ast.TupleType(List.map argType args)
            in
              apply (fn(t) => SOME(Ast.ArrowType(argsType, t))) bodyTypeOpt
            end
          | Ast.Variable r =>
              if IntBinarySet.member(#polymorphic_symbols t, #id (!r))
              then SOME(fresh (Unifier.get (#id (!r))))
              else SOME(Unifier.get (#id (!r)))
          | Ast.LetIn(decls, body) =>
            apply (fn(t) => inferImpl(body, constraint_id)) (multiInferImpl decls)
          | Ast.Valdec(arg, _, body) =>
            apply (fn(t) => Unifier.unify(argType(arg), t)) (inferImpl(body, constraint_id))
          | Ast.IfThenElse(condition, ifExp, elseExp) =>
            let
              val conditionTypeOpt = inferImpl(condition, Unifier.next_id())
              val ifTypeOpt = inferImpl(ifExp, constraint_id)
              val elseTypeOpt = inferImpl(elseExp, constraint_id)
            in
              case (conditionTypeOpt, ifTypeOpt, elseTypeOpt) of
                (SOME conditionType, SOME ifType, SOME elseType) =>
                  apply (fn(_) => Unifier.unify(ifType, elseType)) (Unifier.unify(Ast.BaseType Ast.KBool, conditionType))
              | _ => NONE
            end
      in
        apply (fn(t) => Unifier.unify(Unifier.get constraint_id, t)) optInferedType
      end
  in
    Unifier.reset(#max_symbol_id t);
    inferImpl(#program t, Unifier.next_id())
  end

end
