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
    (*| Tuple of Exp list*)
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

  fun create_symbol(name) = {lab = name, id = 0}

  fun argeq(Name(l), Name(r)) = !l = !r
    ;

  fun listeq(comp)(l, r) = case (l, r)
    of (nil, nil) => true
    | (l1 :: l2, r1 :: r2) => comp(l1, r1) andalso listeq comp (l2, r2)
    | (_, _) => false
    ;

  fun eq(IntConstant(l1), IntConstant(r1)) = l1 = r1
    | eq(StringConstant(l1), StringConstant(r1)) = l1 = r1
    | eq(Unit, Unit) = true
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

  fun toString_arg(Name(r)) = toString_sym (!r)

  fun toString(IntConstant(v)) = Int.toString v
    | toString(StringConstant(v)) = "\"" ^ v ^ "\""
    | toString(Unit) = "()"
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
