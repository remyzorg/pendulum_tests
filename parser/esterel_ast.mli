
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

type decls = decl list

type emodule = {
  name : ident;
  decls : decls;
  program : Pendulum_ast.Ast.Derived.statement;
}

type emodules = emodule list
