

type loc = Pendulum_ast.loc
type 'a location = 'a Pendulum_ast.location
type test = Pendulum_ast.test
type label = Pendulum_ast.label
type exp = Pendulum_ast.exp
type ident = Pendulum_ast.ident
type valued_ident = Pendulum_ast.valued_ident

type estatement = (estatement_tree) location
and estatement_tree =
  | ESloop of estatement
  | ESseq of estatement * estatement
  | ESpar of estatement * estatement
  | ESemit of ident
  | ESnothing
  | ESpause
  | ESsuspend of estatement * test
  | EStrap of label * estatement
  | ESexit of label
  | ESpresent of test * estatement * estatement
  | ESatom of exp
  | ESsignal of valued_ident * estatement
  | ESrun of ident * ident list * loc
  | EShalt
  | ESsustain of valued_ident
  | ESpresent_then of test * estatement
  | ESawait of test
  | ESawait_imm of test
  | ESsuspend_imm of estatement * test
  | ESabort of estatement * test
  | ESweak_abort of estatement * test
  | ESloop_each of estatement * test
  | ESevery of test * estatement


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
  program : estatement;
}

type emodules = emodule list
