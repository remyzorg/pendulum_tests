
open Pendulum_ast

type decl_spec =
  | Doutput
  | Dinput
  | Dinputoutput
  | Dconstant

type decl = {
  spec : decl_spec;
  name : ident;
}


type case = EXTcase

type extended_tests =
  | EXTsignal of ident
  | EXTcases of case list

type decls = decl list

type emodule = {
  name : ident;
  decls : decls;
  program : Pendulum_ast.Derived.statement;
}

type emodules = emodule list
