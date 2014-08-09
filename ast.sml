structure
Ast = struct

  type Symbol = {lab: string, id: int};

  datatype Arg =
      Name of Symbol ref
    (* typed name *)
    ;

  datatype Exp =
      IntConstant of int
    | StringConstant of string
    | Unit
    | Variable of Symbol ref
    | App of Exp * Exp
    | InfixApp of Exp * string * Exp
    | Tuple of Exp list
    (*| Select of int*)
    | Sequence of Exp list
    (* typed expression *)
    (* conjunction & discjunction, possibly as infix applications *)
    | IfThenElse of Exp * Exp * Exp
    | LetIn of Exp list * Exp
    | Fn of Arg list * Exp
    | Valdec of Arg * bool * Exp
    ;

  datatype BaseKind =
      KInt
    | KString
    | KBool
    | KUnit
    ;

  datatype Type =
      BaseType of BaseKind
    | TypeVariable of int
    | ArrowType of Type * Type
    | TupleType of Type list
    ;

  fun create_symbol(name) = {lab = name, id = 0}

  fun listeq(comp)(l, r) = case (l, r)
    of (nil, nil) => true
    | (l1 :: l2, r1 :: r2) => comp(l1, r1) andalso listeq comp (l2, r2)
    | (_, _) => false
    ;

  fun argeq(Name(l), Name(r)) = !l = !r
    ;

  fun eq(IntConstant(l1), IntConstant(r1)) = l1 = r1
    | eq(StringConstant(l1), StringConstant(r1)) = l1 = r1
    | eq(Unit, Unit) = true
    | eq(Tuple(l), Tuple(r)) = listeq eq (l, r)
    | eq(Sequence(l), Sequence(r)) = listeq eq (l, r)
    | eq(Variable(l1), Variable(r1)) = !l1 = !r1
    | eq(App(l1, l2), App(r1, r2)) =
        eq(l1, r1) andalso
        eq(l2, r2)
    | eq(InfixApp(l1, opl, l2), InfixApp(r1, opr, r2)) =
        eq(l1, r1) andalso
        opl = opr andalso
        eq(l2, r2)
    | eq(IfThenElse(l1, l2, l3), IfThenElse(r1, r2, r3)) =
        eq(l1, r1) andalso
        eq(l2, r2) andalso
        eq(l3, r3)
    | eq(LetIn(l1, l2), LetIn(r1, r2)) =
        listeq eq (l1, r1) andalso
        eq(l2, r2)
    | eq(Fn(l1, l2), Fn(r1, r2)) =
        listeq argeq (l1, r1) andalso
        eq(l2, r2)
    | eq(Valdec(l1, l2, l3), Valdec(r1, r2, r3)) =
        argeq(l1, r1) andalso
        l2 = r2 andalso
        eq(l3, r3)
    | eq(_, _) = false
    ;

  fun toString_list(sep)(toString)(ls) = case ls
    of hd :: nil => toString(hd)
    | hd1 :: hd2 :: tail => toString(hd1) ^ sep ^ " " ^ (toString_list sep toString (hd2 :: tail))
    |  nil => ""
    ;

  fun toString_sym(sym: Symbol) =
    if #id sym = 0 then #lab sym else (#lab sym) ^ "_" ^ (Int.toString (#id sym))

  fun niceTvarPrinter() =
    let
      val assigns: (char IntMap.map) ref = ref IntMap.empty
      val next: char ref = ref #"a"
    in
      fn(t) => case t of
        TypeVariable r =>
          let
            val assigned = case IntMap.find(!assigns, r) of
              SOME a => a
            | NONE => (
              assigns := IntMap.insert(!assigns, r, !next);
              next := Char.succ (!next);
              Char.pred (!next)
              )
          in
            "'" ^ (Char.toString assigned)
          end
      | _ => raise (Exceptions.IllegalStateException "unreachable")
    end

  fun simpleTvarPrinter(TypeVariable r) = "'X_" ^ (Int.toString r)
    | simpleTvarPrinter(_) = raise (Exceptions.IllegalStateException "unreachable")

  fun toString_type_helper(printer)(t) = case t of
      BaseType(KInt) => "int"
    | BaseType(KBool) => "bool"
    | BaseType(KString) => "string"
    | BaseType(KUnit) => "unit"
    | TypeVariable(r) => printer t
    | ArrowType(l, r) => (toString_type_helper printer l) ^ " -> " ^ (toString_type_helper printer r)
    | TupleType(l) => toString_list " *" (toString_type_helper printer) l

  fun toString_type(t: Type): string = toString_type_helper simpleTvarPrinter t

  fun toString_arg(Name(r)) = toString_sym (!r)

  fun toString(exp: Exp): string =
    let
      fun precedence(someExp: Exp): int = case someExp of
          IntConstant _ =>    1
        | StringConstant _ => 1
        | Unit =>             1
        | Variable _ =>       1
        | LetIn _ =>          1
        | App _ =>            2
        | Tuple _ =>          2
        | InfixApp _ =>       3
        | IfThenElse _ =>     4
        | Fn _ =>             5
        | Valdec _ =>         6
        | Sequence _ =>       6

      fun parenthise(otherExp) =
        if precedence(exp) <= precedence(otherExp)
        then "(" ^ (toString otherExp) ^ ")"
        else toString otherExp
    in
      case exp of
        IntConstant(v) => Int.toString v
      | StringConstant(v) => "\"" ^ v ^ "\""
      | Unit => "()"
      | Tuple(l) => "(" ^ (toString_list "," parenthise l) ^ ")"
      | Sequence(l) => toString_list ";" parenthise l
      | Variable(r) => toString_sym (!r)
      | App(l, r) => (parenthise l) ^ " " ^ (parenthise r)
      | InfixApp(exp1, ope, exp2) => (parenthise exp1) ^ " " ^ ope ^ " " ^ (parenthise exp2)
      | IfThenElse(c, l, r) => "if " ^ (parenthise c) ^ " then " ^ (parenthise l) ^ " else " ^ (parenthise r)
      | LetIn(decls, body) => "let " ^ (toString_list ";" toString decls) ^ " in " ^ toString(body) ^ " end"
      | Fn(args, body) => "fn(" ^ (toString_list "," toString_arg args) ^ ") => " ^ (parenthise body)
      | Valdec(name, recursive, body) =>
          (if recursive then "fun " else "val ") ^ toString_arg(name) ^ " = " ^ (parenthise body)
    end

end
