
%{
    open Esterel_ast
    open Pendulum_ast
    open Pendulum_ast.Simpl_expr

    module Ast = Pendulum_ast.Derived

    let mk_loc e l = { loc = l; content = e }

    let loc e =
      mk_loc e @@ Location.symbol_rloc ()

    let loc_dummy e =
      mk_loc e Location.none




%}


%token <string> IDENT
%token <int> INTEGER
%token <string> FLOAT
%token <string> STRING

%token MODULE INPUT OUTPUT SENSOR PROCEDURE FUNCTION
%token RELATION INPUTOUTPUT CONSTANT TYPE
%token SIGNAL VAR
%token END DO IN WHEN EACH

%token EMIT LOOP EVERY TIMES TRAP EXIT SUSPEND SUSTAIN ABORT RUN REPEAT UPTO
%token AWAIT PRESENT CALL NOTHING PAUSE HALT THEN IF ELSIF ELSE WEAK POSITIVE

%token LPAR RPAR LSB RSB
%token SEMICOLON COLON COMMA DOT
%token EOF


/* %token <Ast.binop> EQOP
%token <Ast.binop> COMP
%token AMPERSAND
%token AND DOT
%token ARROW
%left COMP
%right EQ
%token LBRACE RBRACE LSQUARE RSQUARE
*/

%token COLONEQ
%token EQ
%token EQGT
%token BARBAR NOT PRE AND OR IMMEDIATE
%token CASE HANDLE
%token PLUS MINUS SHARP
%token IMARK

%right IMARK
%left BARBAR /* || */
%left AND OR
%right NOT
%right IMMEDIATE
%left SEMICOLON
%left EQ
%left PLUS MINUS             /* + - */
%right uminus uplus

/*
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

end_module:
    | DOT        {}
    | END MODULE {}
;

emodule:
    | MODULE; name = ident; COLON; decls = list(decl); program = program; end_module
      { {name; decls; program}  }
;


ident:
    | id = IDENT { loc id }
;

decl_spec:
    | INPUT { Dinput }
    | OUTPUT { Doutput }
    | INPUTOUTPUT { Dinputoutput }
    | SENSOR
      { Pendulum_ast.(test_error (Not_implemeted "sensor")) }
;

signal_relation:
    | hd = ident SHARP tl = separated_nonempty_list(SHARP, ident) { hd :: tl }
    | id1 = ident EQGT id2 = ident { [id1; id2] }
;

val_init:
    | EQ; e = expr
    | COLONEQ; e = expr
      { e }
;

vars_init:
    | names = separated_nonempty_list(COMMA,
        id = ident; v = option(val_init); t = option(COLON; ident { $2 })
          { id, v, t }
      )
      { names }
;


decl:
    | spec = decl_spec ; names = separated_nonempty_list(COMMA,
                           id = ident; option (LPAR; ident; RPAR { $2 }); option(val_init);
                           option(COLON; ident { $2 }) { id }); SEMICOLON
        { Dsig {spec; names} }
    | PROCEDURE; name=ident; LPAR; outs = separated_list (COMMA, ident); RPAR
                         ; LPAR; ins = separated_list (COMMA, ident); RPAR
                         ; SEMICOLON
      { Dprocedure (name, ins, outs) }

    | FUNCTION; name=ident; LPAR; separated_list (COMMA, ident); RPAR
                          ; COLON ident SEMICOLON
      { Pendulum_ast.(test_error (Not_implemeted "function")) }

    | RELATION; separated_nonempty_list(COMMA, signal_relation); SEMICOLON;
      { Pendulum_ast.(test_error (Not_implemeted "signal relations")) }
    | CONSTANT ; names = vars_init; SEMICOLON { Dconstant names }
    | TYPE; id = ident; SEMICOLON { Dtype id }

;

case: CASE; t = test_presence; p = option (DO; p = program { p })
      { t, p }


test_presence_expr:
    | NOT; t = test_presence_expr { EXTnot t }
    | id = ident { EXTsignal id }
    | PRE; LPAR; id = ident; RPAR; { EXTpre id }
    | t1 = test_presence_expr; AND; t2 = test_presence_expr { EXTand (t1, t2) }
    | t1 = test_presence_expr; OR; t2 = test_presence_expr { EXTor (t1, t2) }
    | LSB; t = test_presence_expr; RSB { t }
    | IMMEDIATE; t = test_presence_expr { EXTimm t }
;

test_presence:
    | t = test_presence_expr { t }
    | i = INTEGER; id = ident { EXTtick (id, i) }
;

expr:
    | id = ident { loc @@ Simpl_expr.EXPident id }
    | fn = ident ; LPAR; e = expr; RPAR { loc @@ Simpl_expr.EXPapp (fn, e) }
    | i = INTEGER { loc @@ Simpl_expr.(EXPlit (Linteger i)) }
    | f = FLOAT { loc @@ Simpl_expr.(EXPlit (Lfloat f)) }
    | s = STRING { loc @@ Simpl_expr.(EXPlit (Lstring s)) }
    | e1 = expr; PLUS; e2 = expr { loc @@ Simpl_expr.(EXPop (OPplus, e1, e2)) }
    | e1 = expr; MINUS; e2 = expr { loc @@ Simpl_expr.(EXPop (OPminus, e1, e2)) }
    | MINUS; e = expr %prec uminus { loc @@ Simpl_expr.(EXPunop (Uminus, e)) }
    | PLUS; e = expr %prec uplus { loc @@ Simpl_expr.(EXPunop (Uplus, e)) }
    | IMARK; e = expr { loc @@ Simpl_expr.(EXPunop (Uvalue, e)) }
    | e1 = expr EQ; e2 = expr { loc @@ Simpl_expr.(EXPop (OPeq, e1, e2))  }
    | e1 = expr AND e2 = expr { loc @@ Simpl_expr.(EXPop (OPand, e1, e2))  }
;

label_test_expr:
    | id = ident {  }
    | id1 = ident; AND; id2 = ident {  }
;

elsif:
    | ELSIF; e = expr; THEN p = program
      { e, p }
;

handle_case:
    | HANDLE; label_test_expr; DO; p = program { p }
;

program:
    | NOTHING
      { loc Ast.Nothing }

    | PAUSE
      { loc Ast.Pause }

    | LOOP; p = program; END; option (LOOP)
      { loc @@ Ast.Loop p }

    | LOOP; p = program; EACH; t = test_presence
      { loc @@ Ast.Loop_each (p, Simpl_expr.extract_test t) }

    | p1 = program; SEMICOLON; p2 = program
      {loc @@ Ast.Seq (p1, p2)}

    | p1 = program; BARBAR; p2 = program
      {loc @@ Ast.Par (p1, p2)}

    | LSB; p = program; RSB { p }

    | p = program ; SEMICOLON
      { p }

    | TRAP; vars = vars_init ; IN
          ; p = program
          ; handles = list(handle_case)
          ; END; TRAP
      {
        if handles != [] then Pendulum_ast.(test_error (Not_implemeted "handle trap"))
        else List.fold_left (fun acc (id, v, t) -> loc @@ Ast.Trap (Label id, acc)) p vars
      }

    | ABORT; p = program; WHEN; t = test_presence
      { loc @@ Ast.Abort (p, extract_test t) }

    | WEAK; ABORT; p = program; WHEN; t = test_presence
      { loc @@ Ast.Weak_abort (p, extract_test t) }

    | SUSPEND; p = program; WHEN; t = test_presence
      { loc @@ Ast.Suspend (p, extract_test t)}

    | SUSTAIN; id = ident;
      { loc @@ Ast.Sustain (mk_vid id Pendulum_ast.unit_expr) }

    | EMIT; id = ident;
      { loc @@ Ast.Emit (mk_vid id Pendulum_ast.unit_expr) }

    | EMIT; id = ident; LPAR; e = expr; RPAR
      { loc @@ Ast.Emit (mk_vid id @@ Simpl_expr.to_ocaml e) }

    | EVERY; t = test_presence; DO; p = program; END; option (EVERY)
      { loc @@ Ast.Every (extract_test t, p) }

    | EXIT; id = ident; v = option (LPAR; expr; RPAR { $2 });
      {
        match v with None -> loc @@ Ast.Exit (Label id)
        | Some _ -> Pendulum_ast.(test_error (Not_implemeted "label values"))
      }

    | HALT
      { loc @@ Ast.Halt  }

    | PRESENT; t = test_presence; p_then = option (THEN; p = program { p })
                                ; p_else = option (ELSE; p = program { p })
                                ; END; option (PRESENT)
      { let t = extract_test t in
        loc @@ match p_then, p_else with
        | None, Some p_else -> Ast.Present (t, loc @@ Ast.Nothing, p_else);
        | Some p_then, Some p_else -> Ast.Present (t, p_then, p_else)
        | Some p_then, None -> Ast.Present_then (t, p_then)
        | None, None -> assert false
      }

    | AWAIT; t = test_presence
       { loc @@ Ast.Await (extract_test t) }

    | AWAIT; t = test_presence; DO; p = program; END AWAIT
       { loc @@ Ast.Seq ((loc @@ Ast.Await (extract_test t)), p) }

    | SIGNAL; sigs = vars_init IN; p = program; END; SIGNAL
        {
          List.fold_left (
              fun acc (id, v, _)->
              let v = match v with None -> Pendulum_ast.unit_expr
                                 | Some v -> Simpl_expr.to_ocaml v
              in loc @@ Ast.Signal (mk_vid id v, acc)
            ) p sigs
        }

    | RUN; id = ident
      { Pendulum_ast.(test_error (Not_implemeted "run")) }

    | VAR; vars_init IN; p = program; END; VAR
      { Pendulum_ast.(test_error (Not_implemeted "var")) }

    | IF; e = expr; THEN
                  ; p_then = program
                  ; elsifs = list(elsif)
                  ; p_else = option (ELSE; p_else = program { p_else } )
                  ; END; IF
      { Pendulum_ast.(test_error (Not_implemeted "if-then-else")) }


    | CALL; _id=ident
          ; LPAR; _outs = separated_list(COMMA, ident); RPAR
          ; LPAR; _ins = separated_list(COMMA, expr); RPAR
      { Pendulum_ast.(test_error (Not_implemeted "call procedure")) }

    | option (POSITIVE); REPEAT; i = INTEGER; TIMES; p = program; END; REPEAT
      { Pendulum_ast.(test_error (Not_implemeted "repeat")) }

    | DO program UPTO ident
      { Pendulum_ast.(test_error (Not_implemeted "upto")) }

    | AWAIT nonempty_list (case) END
      { Pendulum_ast.(test_error (Not_implemeted "presence case tests")) }

    | ABORT; p = program; WHEN; nonempty_list (case); END; ABORT;
      { Pendulum_ast.(test_error (Not_implemeted "presence case tests")) }

    | WEAK; ABORT; p = program; WHEN; nonempty_list (case); END; WEAK; ABORT;
      { Pendulum_ast.(test_error (Not_implemeted "presence case tests")) }

    | PRESENT; l = nonempty_list (case) ; END; PRESENT
      { Pendulum_ast.(test_error (Not_implemeted "presence case tests")) }

    | lhs = ident; COLONEQ; rhs = expr
      { Pendulum_ast.(test_error (Not_implemeted "assignation")) }




;
