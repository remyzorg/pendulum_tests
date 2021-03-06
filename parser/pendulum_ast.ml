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
    | Lfloat of string

  type op = OPplus | OPminus | OPeq | OPand
  type unop = Uplus | Uminus | Uvalue

  type esterel_expr = esterel_expr_tree location
  and esterel_expr_tree =
    | EXPident of ident
    | EXPapp of ident * esterel_expr
    | EXPlit of literal
    | EXPop of op * esterel_expr * esterel_expr
    | EXPunop of unop * esterel_expr


  type extended_tests =
    | EXTsignal of ident
    | EXTpre of ident
    | EXTnot of extended_tests
    | EXTand of extended_tests * extended_tests
    | EXTor of extended_tests * extended_tests
    | EXTimm of extended_tests
    | EXTtick of ident * int


  let lit_to_ocaml lit =
    let open Ast_helper in
    match lit with
    | Lstring s -> Exp.constant (Asttypes.Const_string(s, None))
    | Linteger i -> Exp.constant (Asttypes.Const_int i)
    | Lfloat s -> Exp.constant (Asttypes.Const_float s)

  let op_to_ocaml = function
    | OPplus -> [%expr (+)]
    | OPminus -> [%expr (-)]
    | OPeq -> [%expr (=)]
    | OPand -> [%expr (&&)]

  let unop_to_ocaml = function
    | Uplus -> [%expr (+)]
    | Uminus -> [%expr (-)]
    | Uvalue -> [%expr (!!)]

  let rec to_ocaml e = match e.content with
    | EXPident id -> Sync2ml.Ocaml_gen.mk_ident id
    | EXPapp (fn, e) -> [%expr [%e Sync2ml.Ocaml_gen.mk_ident fn] [%e to_ocaml e]]
    | EXPlit lit -> lit_to_ocaml lit
    | EXPop (op, e1, e2) -> [%expr [%e op_to_ocaml op] [to_ocaml e1] [to_ocaml e2]]
    | EXPunop (op, e) -> [%expr [%e unop_to_ocaml op] [to_ocaml e]]

  let extract_test =
    function
    | EXTsignal id -> id, None, None
    | EXTpre _ -> test_error (Not_implemeted "presence 'pre'")
    | EXTnot

 _ -> test_error (Not_implemeted "presence negation")
    | EXTand (t1, t2) -> test_error (Not_implemeted "presence conjonction")
    | EXTor (t1, t2) -> test_error (Not_implemeted "presence disjonction")
    | EXTimm s -> test_error (Not_implemeted "immediate")
    | EXTtick (id, i) -> test_error (Not_implemeted "presence ticks")




end



