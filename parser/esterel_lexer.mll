

{


open Lexing
open Esterel_parser

exception Lexical_error of string

let id_or_keyword =
  let h = Hashtbl.create 17 in
  List.iter (fun (str, kw) -> Hashtbl.add h str kw)
    [ "module", MODULE
    ; "input", INPUT
    ; "sensor", SENSOR
    ; "output", OUTPUT
    ; "inputoutput", INPUTOUTPUT
    ; "end", END
    ; "when", WHEN
    ; "each", EACH
    ; "case", CASE
    ; "handle", HANDLE
    ; "in", IN
    ; "then", THEN
    ; "do", DO
    ; "not", NOT
    ; "pre", PRE
    ; "and", AND
    ; "or", OR
    ; "immediate", IMMEDIATE
    ; "weak", WEAK
    ; "constant", CONSTANT
    ; "procedure", PROCEDURE
    ; "function", FUNCTION
    ; "relation", RELATION
    ; "type", TYPE

    ; "emit", EMIT
    ; "times", TIMES
    ; "loop", LOOP
    ; "every", EVERY
    ; "run", RUN
    ; "trap", TRAP
    ; "upto", UPTO
    ; "exit", EXIT
    ; "suspend", SUSPEND
    ; "signal", SIGNAL
    ; "var", VAR
    ; "abort", ABORT
    ; "repeat", REPEAT
    ; "positive", POSITIVE
    ; "await", AWAIT
    ; "if", IF
    ; "elsif", ELSIF
    ; "else", ELSE
    ; "present", PRESENT
    ; "call", CALL
    ; "nothing", NOTHING
    ; "pause", PAUSE
    ; "halt", HALT
    ; "sustain", SUSTAIN
    ];
  fun s -> try Hashtbl.find h s with Not_found -> IDENT s

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with pos_lnum = pos.pos_lnum + 1
           ; pos_bol = pos.pos_cnum
           ; pos_cnum = 0;}

let char_error s = raise (Lexical_error ("illegal character sequence : " ^ s))

let decode_char s =
  match Bytes.length s with
  | 1 -> Char.code s.[0]
  | 2 | 4 when s.[0] == '\\' -> begin
      match s.[1] with
      | 'n' -> 10
      | 't' -> 9
      | '\'' -> 39
      | '\"' -> 34
      | 'x' -> Bytes.set s 0 '0'; (int_of_string s)
      | _ -> char_error s
    end
  | _ -> char_error s

let str_buff = Buffer.create 512


}


let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | '_') (alpha | '_' | digit)*
let hexa = digit | ['a'-'f' 'A'-'F']
let char =
  [^'\000'-'\x1f' '\\' '\'' '\"']
  | '\\' ('n' | 't' | '\'' |'\"')
  | "\\x" hexa hexa


rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | "//" [^'\n']* ('\n' | eof)
      { newline lexbuf; token lexbuf }
  | "%" [^'\n']* ('\n' | eof)
      { newline lexbuf; token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | digit+ as s
      {
	try
	  INTEGER (int_of_string s)
	with _ ->
	  raise (Lexical_error ("invalid integer constant '" ^ s ^ "'"))
      }
  | (digit+ "." digit+) as s
      {
	try
	  FLOAT s
	with _ ->
	  raise (Lexical_error ("invalid float constant '" ^ s ^ "'"))
      }
  | ((digit+ "." digit+) as s) "f"
      {
	try
	  FLOAT s
	with _ ->
	  raise (Lexical_error ("invalid float constant '" ^ s ^ "'"))
      }
  | '\'' (char as s) '\''
      { INTEGER (decode_char s) }
  | '\"'
      { Buffer.reset str_buff;
        string lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LSB }
  | ']' { RSB }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '.' { DOT }
  | ':' { COLON }
  | "-" { MINUS }
  | "+" { PLUS }
  | "?" { IMARK }
  | "||" { BARBAR }
  | "#" { SHARP }
  | ":=" { COLONEQ }
  | "=" { EQ }
  | "=>" { EQGT }
  | eof { EOF }
  | _
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }
and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
  | _    { comment lexbuf }


and string = parse
  | char as c { Buffer.add_char str_buff (Char.chr (decode_char c));
                string lexbuf }
  | '\"' { STRING (Buffer.contents str_buff) }
  | eof  { raise (Lexical_error "unterminated string") }
  | _ { char_error (lexeme lexbuf) }
