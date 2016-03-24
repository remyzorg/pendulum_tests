
open Pendulum_ast



type decl_spec =
  | Doutput
  | Dinput
  | Dinputoutput
  | Dconstant
  | Dprocedure of ident list * ident list

type decl = {
  spec : decl_spec;
  name : ident;
}


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
