
%{
    open Esterel_ast
    open Pendulum_ast

    let mk_loc e l = { loc = l; content = e }

    let loc e =
      mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

    let loc_i i e =
      mk_loc e (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

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
%token END
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

/* Symboles */

%token LPAR RPAR LBRACE RBRACE LSQUARE RSQUARE
%token SEMICOLON COMMA DOT ARROW
%token EOF

/* Opérateurs */

%token EQ
%token OR
%token AND
/* %token <Ast.binop> EQOP */
/* %token <Ast.binop> COMP */
%token PLUS MINUS
%token STAR SLASH PERCENT
%token PLUSPLUS MINUSMINUS BANG IMARK AMPERSAND

/*s Précédences */

%nonassoc then
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

/*s Point d'entrée */

%start modules
%type <Esterel_ast.emodules> modules

%%

modules:
| EOF { [] }
