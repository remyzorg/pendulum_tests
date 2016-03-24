
%{
    open Esterel_ast
    open Pendulum_ast

    module Ast = Pendulum_ast.Derived


    let mk_loc e l = { loc = l; content = e }

    let loc e =
      mk_loc e @@ Location.symbol_rloc ()

    let loc_dummy e =
      mk_loc e Location.none


    let extract_test = function
      | EXTsignal s -> s, None, None
      | EXTcases l -> failwith "Not implemented yet"



%}


%token <string> IDENT
%token <int> INTEGER
%token <string> STRING


%token MODULE
%token INPUT
%token OUTPUT
%token PROCEDURE
%token INPUTOUTPUT
%token END
%token DO
%token IN
%token WHEN
%token CONSTANT
%token EMIT
%token LOOP
%token EVERY
%token TIMES
%token TRAP
%token EXIT
%token SUSPEND
%token ABORT
%token REPEAT

%token PRESENT
%token CALL
%token NOTHING
%token PAUSE
%token HALT
%token THEN
%token IF
%token ELSIF
%token ELSE


%token LPAR RPAR
%token SEMICOLON COLON COMMA 
%token EOF


/* %token <Ast.binop> EQOP
%token <Ast.binop> COMP
%token AMPERSAND
%token AND DOT
%token ARROW
%left COMP
%right EQ
%token EQ
%token LBRACE RBRACE LSQUARE RSQUARE
*/

%token COLONEQ
%token OR
%token CASE
%token PLUS MINUS
%token IMARK

%right IMARK
%left OR /* || */
%left SEMICOLON
%left PLUS MINUS             /* + - */

/*
%left COLONEQ
%nonassoc THEN
%nonassoc ELSE
%left CASE
%right AMPERSAND
%left AND     &&
%left ARROW
%left LSQUARE par_expr
*/



%start emodules
%type <Esterel_ast.emodules> emodules

%%


emodules: | l = list(emodule); EOF { l } ;

emodule:
    | MODULE; name = ident; COLON; decls = list(decl); program = program; END; MODULE
      { {name; decls; program}  }
;


ident:
    | id = IDENT { loc id }
;

decl_spec:
    | INPUT { Dinput }
    | OUTPUT { Doutput }
    | CONSTANT { Dconstant }
    | INPUTOUTPUT { Dinputoutput }
;


decl:
    | spec = decl_spec ; name = ident; COLON; _t = IDENT; SEMICOLON
      { {spec; name} }
    | spec = decl_spec ; name = ident; SEMICOLON
      { {spec; name} }
    | PROCEDURE; name=ident; LPAR; outs = separated_list (COMMA, ident); RPAR
                         ; LPAR; ins = separated_list (COMMA, ident); RPAR
                         ; SEMICOLON
      { { spec = Dprocedure (ins, outs); name } }
;

_case:
    | CASE; id = ident; DO; p = program
      { EXTcase };

test_presence:
/*    | l = nonempty_list(case) { EXTcases l } */
    | id = ident { EXTsignal id }
;

expr:
    | id = ident { loc @@ Simpl_expr.EXPident id }
    | IMARK; e = expr { loc @@ Simpl_expr.EXPvalue e }
    | fn = ident ; LPAR; e = expr; RPAR { loc @@ Simpl_expr.EXPapp (fn, e) }
    | i = INTEGER { loc @@ Simpl_expr.(EXPlit (Linteger i)) }
    | s = STRING { loc @@ Simpl_expr.(EXPlit (Lstring s)) }
    | e1 = expr; PLUS; e2 = expr { loc @@ Simpl_expr.(EXPop (OPplus, e1, e2)) }
    | e1 = expr; MINUS; e2 = expr { loc @@ Simpl_expr.(EXPop (OPminus, e1, e2)) }
;


elsif:
    | ELSIF; e = expr; THEN p = program
      { (e, p) }
;


program:
    | NOTHING
      { loc Ast.Nothing }


    | PAUSE
      { loc Ast.Pause }

    | LOOP; p = program; END
      { loc @@ Ast.Loop p }

    | p1 = program; SEMICOLON; p2 = program
      {loc @@ Ast.Seq (p1, p2)}

    | p1 = program; OR; p2 = program
      {loc @@ Ast.Par (p1, p2)}

    | p = program ; SEMICOLON
      { p }

    | TRAP; id = ident; IN; p = program; END; TRAP
      { loc @@ Ast.Trap (Label id, p) }

    | ABORT; p = program; WHEN; t = test_presence; END; ABORT;
      { loc @@ Ast.Abort (p, extract_test t) }

    | SUSPEND; p = program; WHEN; t = test_presence
      { loc @@ Ast.Suspend (p, extract_test t)}

    | lhs = ident; COLONEQ; rhs = ident
      { assert false (* not implemented yet *) }

    | EMIT; id = ident;
      { loc @@ Ast.Emit (mk_vid id Pendulum_ast.unit_expr) }

    | EMIT; id = ident; LPAR; e = expr; RPAR
      { loc @@ Ast.Emit (mk_vid id @@ Simpl_expr.to_pendulum e) }

    | EVERY; t = test_presence; DO; p = program; END; EVERY
      { loc @@ Ast.Every (extract_test t, p) }

    | EXIT; id = ident
      { loc @@ Ast.Exit (Label id) }

    | HALT
      { loc @@ Ast.Halt  }

    | PRESENT; t = test_presence; THEN; p_then = program
                                ; p_else = option (ELSE; p_else = program { p_else });
                                ; END; PRESENT;
      { loc @@ match p_else with
               | Some p_else -> Ast.Present (extract_test t, p_then, p_else)
               | None -> Ast.Present_then (extract_test t, p_then)
      }

    (* | PRESENT; t = test_presence; THEN; p_then = program *)
    (*                             ; END; PRESENT; *)
    (*   { loc @@ Ast.Present_then (extract_test t, p_then) } *)

    | IF; e = expr; THEN
                  ; p_then = program
                  ; elsifs = list(elsif)
                  ; p_else = option (ELSE; p_else = program { p_else } )
                  ; END; IF
      { assert false (* not implemented yet *) }


    | CALL; _id=ident
          ; LPAR; _outs = separated_list(COMMA, ident); RPAR
          ; LPAR; _ins = separated_list(COMMA, expr); RPAR
      { assert false (* not implemented yet *) }

    | REPEAT; i = INTEGER; TIMES; p = program; END; REPEAT
       { assert false (*not implemented yet *) }



;
