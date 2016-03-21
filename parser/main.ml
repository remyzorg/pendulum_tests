
open Format
open Lexing

let usage = ""
let spec = []


let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".strl") then
      raise (Arg.Bad "no .strl extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc


let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Esterel_parser.emodules Esterel_lexer.token lb in
    assert false
  with
  | _ -> assert false
