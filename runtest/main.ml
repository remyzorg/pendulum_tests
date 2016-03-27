
open Format
open Lexing

let printf = ANSITerminal.printf
let normal = []
let okstyle = ANSITerminal.([green])
let kostyle = ANSITerminal.([red])

let usage = ""

let hideok = ref false
let noimpl = ref false

let spec = [
  "-nook", Arg.Set hideok, " ";
  "-noimpl", Arg.Set noimpl, " ";
]

let dirs = ref []
let add_dir s = dirs := s :: !dirs
let () = Arg.parse spec add_dir usage


let (!%) = let revarg g a b = g b a in revarg

let report_loc file (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  printf normal "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let test_files testdir =
  Sys.readdir testdir
  |> Array.to_list
  |> List.filter (!%Filename.check_suffix "strl")
  |> List.map ((^) Filename.(basename testdir ^ dir_sep))

let parse max acc f =
  let testname = Filename.(chop_extension @@ basename f) in
  let testprompt = Format.sprintf "%s:%s" testname
      (String.(make (max - length testname - 5) ' '))
  in
  let c = open_in f in
  let lb = Lexing.from_channel c in
  let result =
    try
      let p = Esterel_parser.emodules Esterel_lexer.token lb in
      if not !hideok then printf okstyle "%sOK\n" testprompt;
      p :: acc
    with
    | Pendulum_ast.Test_error err ->
      if not !noimpl then begin
        printf [ANSITerminal.yellow] "%s~~\t" testprompt;
        printf normal "%s\n" (Format.asprintf "%a" Pendulum_ast.print_test_error err)
      end; acc
    | e -> printf kostyle "%sKO: \t" testprompt;
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

let runtests testdir =
  try
    printf normal "-------------------------------------------------\n";
    let testfiles = test_files testdir in
    let max_name_length = List.fold_left
        (fun acc x -> max acc (String.length x)) 0 testfiles
    in
    let test_asts = List.fold_left (parse max_name_length) [] testfiles in
    printf [ANSITerminal.yellow] "Result: %d/%d\n"
      (List.length test_asts) (List.length testfiles)
  with e -> printf normal "%s\n"(Printexc.to_string e)


let () =
  List.iter runtests !dirs



