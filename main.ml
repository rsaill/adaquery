let split s = Str.split_delim (Str.regexp_string ".") s

(* Option *)

type t_action =
  | Index
  | Search of Datatypes.path
  | Locate of Datatypes.path
  | Print of Datatypes.path

let action = ref Index
let project = ref "myproject" 
let files_to_index = ref []

let get_cache_file () =
  let dir =
    try (Unix.getenv "HOME") ^ "/.adaquery"
    with Not_found -> "."
  in
  (if not (Sys.file_exists dir) then
     if Sys.command ("mkdir " ^ dir) = 0 then
       Print.debug "Directory '%s' was created." dir
     else Print.debug "Fail to create directory '%s'." dir);
  Printf.sprintf "%s/%s.cache" dir !project

(* Indexing *)

let get_loc (lb:Lexing.lexbuf) : string * int * int =
  let open Lexing in
  let loc = lb.lex_curr_p in
  ( loc.pos_fname, loc.pos_lnum, (loc.pos_cnum-loc.pos_bol+1) )

let parse (tbl:Table.t) (lb:Lexing.lexbuf) : unit =
  try
    Print.debug "Processing file '%s' ..." lb.Lexing.lex_curr_p.Lexing.pos_fname;
    List.iter (Table.add_decl tbl) (Parser.goal_symbol Lexer.token lb);
  with
    | Parser.Error ->
      begin
        let (fn,l,c) = get_loc lb in
        Print.debug "[file:%s;line:%i;column:%i] Parsing error: unexpected token '%s'." fn l c (Lexing.lexeme lb)
      end
    | Lexer.Error (loc,msg) ->
      begin
        let (fn,l,c) = get_loc lb in
        Print.debug "[file:%s;line:%i;column:%i] Lexing error: %s." fn l c msg
      end

let fullname fn =
  if Filename.is_relative fn then
    (Sys.getcwd ()) ^ "/" ^ fn
  else fn

let index_file (tbl:Table.t) (file:string) : unit =
  let input = open_in file in
  let lb = Lexing.from_channel input in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = fullname file; };
  parse tbl lb

(* Main *)

let args = [
  "-p", Arg.Set_string project ,"Set the project to use";
  "-v", Arg.Set Print.verbose_mode ,"Verbose mode";
  "-index", Arg.Unit (fun _ -> action := Index) ,"Index a list of files";
  "-locate", Arg.String (fun s -> action := Locate (split s)) ,"Locate an object or a package";
  "-search", Arg.String (fun s -> action := Search (split s)) ,"Search for objects or packages matching a prefix";
  "-print", Arg.String (fun s -> action := Print (split s)) ,"Print the content of a package";
]

let _ =
  try
    Arg.parse args (fun fn -> files_to_index := fn :: !files_to_index) "adaquery -p myproject -index files\nadaquery -p myproject (-locate|-search|-print) decl";
    begin match !action with
      | Index ->
        let tbl = Table.create () in
        List.iter (index_file tbl) !files_to_index;
        Table.write tbl (get_cache_file ())
      | Locate s ->
        let tbl = Table.read (get_cache_file ()) in
        let lst = Table.locate tbl s in
        let print_loc lc =
          Printf.printf "%s\n%i\n" lc.Lexing.pos_fname lc.Lexing.pos_lnum
        in
        List.iter print_loc lst
      | Search s ->
        let tbl = Table.read (get_cache_file ()) in
        let lst = Table.search tbl s in
        List.iter print_endline lst
      | Print s ->
        let tbl = Table.read (get_cache_file ()) in
        Table.print tbl s
    end
  with
  | Sys_error err   -> Print.debug "ERROR %s." err; exit 1
