module Sync2ml = Pendulum_compiler.Sync2ml

include Sync2ml.Ast

type test_error = Not_implemeted of string

exception Test_error of test_error

let print_test_error fmt = function
  | Not_implemeted s -> Format.fprintf fmt "Not implemented yet : %s" s

let test_error e = raise (Test_error e)


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


  type extended_tests =
    | EXTsignal of ident
    | EXTnot of extended_tests
    | EXTand of extended_tests * extended_tests
    | EXTor of extended_tests * extended_tests
    | EXTtick of int


  let lit_to_ocaml lit =
    let open Ast_helper in
    match lit with
    | Lstring s -> Exp.constant (Asttypes.Const_string(s, None))
    | Linteger i -> Exp.constant (Asttypes.Const_int i)

  let rec to_ocaml e = match e.content with
    | EXPident id -> Sync2ml.Ocaml_gen.mk_ident id
    | EXPapp (fn, exp) -> [%expr [%e Sync2ml.Ocaml_gen.mk_ident fn] [%e to_ocaml exp]]
    | EXPvalue vexpr -> [%expr !![%e to_ocaml vexpr]]
    | EXPlit lit -> lit_to_ocaml lit
    | _ -> assert false (* not implemented yet *)

  let extract_test =
    function
    | EXTsignal s -> s, None, None
    | EXTnot signal -> test_error (Not_implemeted "presence negation")
    | EXTand (t1, t2) -> test_error (Not_implemeted "presence conjonction")
    | EXTor (t1, t2) -> test_error (Not_implemeted "presence disjonction")
    | EXTtick i -> test_error (Not_implemeted "presence ticks")




end



