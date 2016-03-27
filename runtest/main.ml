
open Format
open Lexing

let printf = ANSITerminal.printf
let normal = []
let okstyle = ANSITerminal.([green])
let kostyle = ANSITerminal.([red;])



let (!%) = let revarg g a b = g b a in revarg

let testdir = "tests"

let report_loc file (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  printf normal "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let test_files =
  Sys.readdir testdir
  |> Array.to_list
  |> List.filter (!%Filename.check_suffix "strl")
  |> List.map ((^) "tests/")

let max_name_length = List.fold_left
    (fun acc x -> max acc (String.length x)) 0 test_files

let parse acc f =
  let testname = Filename.(chop_extension @@ basename f) in
  printf normal "%s:%s" testname (String.(make (max_name_length - length testname) ' '));
  let c = open_in f in
  let lb = Lexing.from_channel c in
  let result =
    try
      let p = Esterel_parser.emodules Esterel_lexer.token lb in
      printf okstyle "OK\n";
      p :: acc
    with
    | e -> printf kostyle "KO\n";
      begin match e with
        | Esterel_lexer.Lexical_error s ->
          report_loc f (lexeme_start_p lb, lexeme_end_p lb);
          printf normal "lexical error: %s\n" s
        | Esterel_parser.Error ->
          report_loc f (lexeme_start_p lb, lexeme_end_p lb);
          printf normal "Syntax error\n"
        | e -> printf normal "%s\n" (Printexc.to_string e)
      end; acc
  in close_in c; result


let test_asts = List.fold_left parse [] test_files

let () =
  printf [ANSITerminal.yellow] "Result: %d/%d"
    (List.length test_asts) (List.length test_files)


