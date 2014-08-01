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
    (* sequence *)
    (* typed expression *)
    (* conjunction & discjunction, possibly as infix applications *)
    | IfThenElse of Exp * Exp * Exp
    | LetIn of Exp list * Exp
    | Fn of Arg list * Exp
    | Valdec of Arg * Exp
    | Fundec of Arg * Exp
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

  fun typeeq(BaseType kl, BaseType kr) = kl = kr
    | typeeq(TypeVariable l, TypeVariable r) = l = r
    | typeeq(ArrowType(l1, l2), ArrowType(r1, r2)) =
        typeeq(l1, r1) andalso
        typeeq(l2, r2)
    | typeeq(TupleType l, TupleType r) = listeq typeeq (l, r)
    | typeeq(_, _) = false
    ;

  fun argeq(Name(l), Name(r)) = !l = !r
    ;

  fun eq(IntConstant(l1), IntConstant(r1)) = l1 = r1
    | eq(StringConstant(l1), StringConstant(r1)) = l1 = r1
    | eq(Unit, Unit) = true
    | eq(Tuple(l), Tuple(r)) = listeq eq (l, r)
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
    | eq(Valdec(l1, l2), Valdec(r1, r2)) =
        argeq(l1, r1) andalso
        eq(l2, r2)
    | eq(Fundec(l1, l2), Fundec(r1, r2)) =
        argeq(l1, r1) andalso
        eq(l2, r2)
    | eq(_, _) = false
    ;

  fun toString_list(sep)(toString)(ls) = case ls
    of hd :: nil => toString(hd)
    | hd1 :: hd2 :: tail => toString(hd1) ^ sep ^ " " ^ (toString_list sep toString (hd2 :: tail))
    |  nil => ""
    ;

  fun toString_sym(sym: Symbol) =
    if #id sym = 0 then #lab sym else (#lab sym) ^ "_" ^ (Int.toString (#id sym))

  fun toString_type(BaseType(KInt)) = "int"
    | toString_type(BaseType(KBool)) = "bool"
    | toString_type(BaseType(KString)) = "string"
    | toString_type(BaseType(KUnit)) = "unit"
    | toString_type(TypeVariable(r)) = "'X_" ^ Int.toString r
    | toString_type(ArrowType(l, r)) = toString_type(l) ^ " -> " ^ toString_type(r)
    | toString_type(TupleType(l)) = toString_list "*" toString_type l
    ;

  fun toString_arg(Name(r)) = toString_sym (!r)

  fun toString(IntConstant(v)) = Int.toString v
    | toString(StringConstant(v)) = "\"" ^ v ^ "\""
    | toString(Unit) = "()"
    | toString(Tuple(l)) = "(" ^ (toString_list "," toString l) ^ ")"
    | toString(Variable(r)) = toString_sym (!r)
    | toString(App(l, r)) = toString(l) ^ " " ^ toString(r)
    | toString(InfixApp(exp1, ope, exp2)) = toString(exp1) ^ " " ^ ope ^ " " ^ toString(exp2)
    | toString(IfThenElse(c, l, r)) = "if " ^ toString(c) ^ " then " ^ toString(l) ^ " else " ^ toString(r)
    | toString(LetIn(decls, body)) = "let " ^ (toString_list ";" toString decls) ^ " in " ^ toString(body) ^ " end"
    | toString(Fn(args, body)) = "fn(" ^ (toString_list "," toString_arg args) ^ ") => " ^ toString(body)
    | toString(Valdec(name, body)) = "val " ^ toString_arg(name) ^ " = " ^ toString(body)
    | toString(Fundec(name, body)) = "fun " ^ toString_arg(name) ^ " = " ^ toString(body)
    ;

end
