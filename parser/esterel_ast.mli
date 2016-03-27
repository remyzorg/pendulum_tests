
open Pendulum_ast



type decl_spec =
  | Doutput
  | Dinput
  | Dinputoutput
  | Dconstant

type decl_var = {
  spec : decl_spec;
  names : ident list;
}

type decl = Dvar of decl_var | Dprocedure of ident * ident list * ident list

type case = EXTcase

type extended_tests =
  | EXTsignal of ident
  | EXTnot of extended_tests
  (* | EXTcases of case list *)

type decls = decl list

type emodule = {
  name : ident;
  decls : decls;
  program : Pendulum_ast.Derived.statement;
}

type emodules = emodule list
