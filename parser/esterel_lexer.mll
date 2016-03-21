

{


open Lexing
open Esterel_parser

exception Lexical_error of string

let id_or_keyword =
  let h = Hashtbl.create 17 in
  List.iter (fun (str, kw) -> Hashtbl.add h str kw)
    [ "module", MODULE
    ; "input", INPUT
    ; "output", OUTPUT
    ; "end", END
    ; "constant", CONSTANT

    ; "emit", EMIT
    ; "loop", LOOP
    ; "every", EVERY
    ; "trap", TRAP
    ; "exit", EXIT
    ; "suspend", SUSPEND
    ; "abort", ABORT
    ; "repeat", REPEAT
    ; "ifstatement", IFSTATEMENT
    ; "present", PRESENT
    ; "procedureCall", PROCEDURECALL
    ; "assignement", ASSIGNEMENT
    ; "sustain", SUSTAIN
    ; "nothing", NOTHING
    ; "pause", PAUSE
    ; "halt", HALT
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
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | digit+ as s
      {
	try
	  INTEGER (Int32.of_string s)
	with _ ->
	  raise (Lexical_error ("invalid integer constant '" ^ s ^ "'"))
      }
  | '\'' (char as s) '\''
      { INTEGER (Int32.of_int (decode_char s)) }
  | '\"'
      { Buffer.reset str_buff;
        string lexbuf }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
  | '('
      { LPAR }
  | ')'
      { RPAR }
  | '['
      { LSQUARE }
  | ']'
      { RSQUARE }
  | ','
      { COMMA }
  | ';'
      { SEMICOLON }
  | '.'
      { DOT }
  | "->"
      { ARROW }
  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }
  | "%"
      { PERCENT }
  | "!"
      { BANG }
  | "!"
      { IMARK }
  | "&"
      { AMPERSAND }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "="
      { EQ }
  (* | ">" *)
  (*     { COMP Bgt } *)
  (* | ">=" *)
  (*     { COMP Bge } *)
  (* | "<" *)
  (*     { COMP Blt } *)
  (* | "<=" *)
  (*     { COMP Ble } *)
  (* | "==" *)
  (*     { EQOP Beq } *)
  (* | "!=" *)
  (*     { EQOP Bneq } *)
  | "++"
      { PLUSPLUS }
  | "--"
      { MINUSMINUS }
  | eof
      { EOF }
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
