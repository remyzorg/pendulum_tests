module Sync2ml = Pendulum_compiler.Sync2ml

include Sync2ml.Ast

let unit_expr = [%expr ()]

module Simpl_expr = struct

  type esterel_expr =
    | EXPident of ident
    | EXPapp of ident * esterel_expr
    | EXPvalue of esterel_expr

  let rec to_pendulum = function
    | EXPident id -> Sync2ml.Ocaml_gen.mk_ident id
    | EXPapp (fn, exp) -> [%expr [%e Sync2ml.Ocaml_gen.mk_ident fn] [%e to_pendulum exp]]
    | EXPvalue vexpr -> [%expr !![%e to_pendulum vexpr]]



end



