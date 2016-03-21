
%{
    open Esterel_ast
    open Pendulum_ast

    module Ast = Pendulum_ast.Ast.Derived

    let mk_loc e l = { loc = l; content = e }

    let loc e =
      mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

    let loc_dummy e =
      mk_loc e (Lexing.dummy_pos, Lexing.dummy_pos)

%}


%token <string> IDENT
%token <int32> INTEGER
%token <string> STRING

/* Mots clés */

%token MODULE
%token INPUT
%token OUTPUT
%token INPUTOUTPUT
%token END
%token DO
%token IN
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
%token ASSIGNEMENT
%token SUSTAIN
%token NOTHING
%token PAUSE
%token HALT


%token LPAR RPAR LBRACE RBRACE LSQUARE RSQUARE
%token SEMICOLON COLON COMMA DOT ARROW
%token EOF


%token EQ
%token OR
%token AND
/* %token <Ast.binop> EQOP */
/* %token <Ast.binop> COMP */
%token PLUS MINUS
%token STAR SLASH PERCENT
%token PLUSPLUS MINUSMINUS BANG IMARK AMPERSAND

%nonassoc THEN
%nonassoc ELSE

%right EQ
%left OR /* || */
%left AND     /* && */
%left EQOP                   /* == != */
%left COMP                   /* < <= > >= */
%left PLUS MINUS             /* + - */
%left STAR SLASH PERCENT     /* * / % */
%right ustar uminus uplus PLUSPLUS MINUSMINUS BANG AMPERSAND
                             /* + - ++ -- ! & */
%left DOT ARROW LSQUARE par_expr


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
    | spec = decl_spec ; name = ident; _t = IDENT; SEMICOLON
      { {spec; name} }
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

 /*   | SUSPEND; p = program; WHEN; id = ident
      { loc @@  }
      */


;
