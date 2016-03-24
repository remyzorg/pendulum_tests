module Sync2ml = Pendulum_compiler.Sync2ml

include Sync2ml.Ast

let unit_expr = [%expr ()]

module Simpl_expr = struct

  type literal =
    | Lstring of string
    | Linteger of int

  type op = OPplus | OPminus

  type esterel_expr = esterel_expr_tree location
  and esterel_expr_tree =
    | EXPident of ident
    | EXPapp of ident * esterel_expr
    | EXPvalue of esterel_expr
    | EXPlit of literal
    | EXPop of op * esterel_expr * esterel_expr

  let rec to_pendulum e = match e.content with
    | EXPident id -> Sync2ml.Ocaml_gen.mk_ident id
    | EXPapp (fn, exp) -> [%expr [%e Sync2ml.Ocaml_gen.mk_ident fn] [%e to_pendulum exp]]
    | EXPvalue vexpr -> [%expr !![%e to_pendulum vexpr]]
    | _ -> assert false (* not implemented yet *)



end



