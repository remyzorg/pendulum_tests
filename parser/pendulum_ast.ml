


(* type PAst = Pendulum_compiler.Ast *)

module EsterelLoc = struct
  type t = Lexing.position * Lexing.position
  let none = Lexing.dummy_pos, Lexing.dummy_pos
end

module Expression = struct
  type t = Parsetree.expression
  type core_type = Parsetree.core_type
  let print = Pprintast.expression
  module Location = EsterelLoc
end


module Ast = Pendulum_compiler.Ast.Make (Expression)

include Ast
