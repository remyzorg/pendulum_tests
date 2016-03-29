
open Pendulum_ast



type decl_spec =
  | Doutput
  | Dinput
  | Dinputoutput

type decl_sig = {
  spec : decl_spec;
  names : ident list;
}

type decl =
  | Dsig of decl_sig
  | Dprocedure of ident * ident list * ident list
  | Dconstant of (ident * Simpl_expr.esterel_expr option * ident option) list
  | Dtype of ident

type case = EXTcase

type decls = decl list

type emodule = {
  name : ident;
  decls : decls;
  program : Pendulum_ast.Derived.statement;
}

type emodules = emodule list
