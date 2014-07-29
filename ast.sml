structure
Ast = struct

  datatype Arg =
      Name of string
    (* typed name *)
    ;

  datatype Exp =
      IntConstant of int
    | StringConstant of string
    | Variable of string
    | App of Exp * Exp
    | InfixApp of Exp * string * Exp
    | Tuple of Exp list
    | Select of int
    (* sequence *)
    (* typed expression *)
    (* conjunction & discjunction, possibly as infix applications *)
    | IfThenElse of Exp * Exp * Exp
    | LetIn of Exp list * Exp
    | Fn of Arg list * Exp
    | Valdec of Arg * Exp
    (* TupleValdec of Arg list * Exp *)
    | Fundec of Arg list * Exp
    ;

end
