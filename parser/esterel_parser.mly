
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
%token <int32> INTEGER
%token <string> STRING


%token MODULE
%token INPUT
%token OUTPUT
%token INPUTOUTPUT
%token END
%token DO
%token IN
%token WHEN
%token CONSTANT
%token EMIT
%token LOOP
%token EVERY
%token TRAP
%token EXIT
%token SUSPEND
%token ABORT
%token REPEAT
%token IFSTATEMENT
%token PRESENT
%token PROCEDURECALL
%token SUSTAIN
%token NOTHING
%token PAUSE
%token HALT
%token THEN
%token ELSE


%token LPAR RPAR LBRACE RBRACE LSQUARE RSQUARE
%token SEMICOLON COLON COMMA DOT
%token EOF


/* %token <Ast.binop> EQOP
%token <Ast.binop> COMP
%token AMPERSAND
%token AND
%token ARROW
*/

%token EQ
%token COLONEQ
%token OR
%token CASE
%token PLUS MINUS
%token STAR SLASH PERCENT
%token PLUSPLUS MINUSMINUS IMARK

%nonassoc THEN
%nonassoc ELSE

%right EQ
%left CASE
%left OR /* || */
%left SEMICOLON
%left COLONEQ                   /* ==:  */
%left EQOP                   /* == != */
%left COMP                   /* < <= > >= */
%left PLUS MINUS             /* + - */
%left STAR SLASH PERCENT     /* * / % */
%right uminus uplus PLUSPLUS MINUSMINUS
/*
%right AMPERSAND
%left AND     &&
%left ARROW
*/
                             /* + - ++ -- ! & */
%left DOT LSQUARE par_expr



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
;

case:
    | CASE; id = ident; DO; p = program
      { EXTcase };

test_presence:
/*    | l = nonempty_list(case) { EXTcases l } */
    | id = ident { EXTsignal id }
;

expr:
    | id = ident { Simpl_expr.EXPident id }
    | IMARK; e = expr { Simpl_expr.EXPvalue e }
    | fn = ident ; LPAR; e = expr; RPAR { Simpl_expr.EXPapp (fn, e) }
;


program:
    | NOTHING
      { loc Ast.Nothing }

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
                                ; ELSE; p_else = program;
                                ; END; PRESENT;
      { loc @@ Ast.Present ( extract_test t, p_then, p_else) }

    | PRESENT; t = test_presence; THEN; p_then = program
                                ; END; PRESENT;
      { loc @@ Ast.Present_then (extract_test t, p_then) }

;
