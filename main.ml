open Common
let split s = Str.split_delim (Str.regexp_string ".") s

(* Option *)

type t_action =
  | Index
  | Search of path
  | Locate of path
  | Print of path
  | Update

let action = ref Index
let project = ref "myproject" 
let files_to_index = ref []
let check_ext = ref false
let scope = ref []

(* Indexing *)

let parse (tbl:Table.toplevel_tree) (lb:Lexing.lexbuf) : unit =
  try
    Print.debug "Processing file '%s' ..." lb.Lexing.lex_curr_p.Lexing.pos_fname;
    List.iter (Table.add_decl tbl) (Parser.goal_symbol Lexer.token lb);
  with
    | Parser.Error ->
      Print.debug_with_loc lb.Lexing.lex_curr_p
        "Parsing error: unexpected token '%s'." (Lexing.lexeme lb)
    | Lexer.Error (loc,msg) ->
      Print.debug_with_loc lb.Lexing.lex_curr_p "Lexing error: %s." msg

let check_extension filename =
  (not !check_ext)
  || (Filename.check_suffix filename ".ads")

let rec index_file (tbl:Table.toplevel_tree) (file:string) : unit =
  try
    begin
      if Sys.is_directory file then
        Array.iter (fun f -> index_file tbl (file^"/"^f)) (Sys.readdir file)
      else
      if check_extension file then
        begin
          let input = open_in file in
          let lb = Lexing.from_channel input in
          lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with
                                    Lexing.pos_fname = Files.fullname file; };
          parse tbl lb
        end
    end
  with Sys_error err -> Print.fail "Error: %s" err

(* Main *)

let args = [
  "-p", Arg.Set_string project ,"Set the project to use";
  "-v", Arg.Set Print.verbose_mode ,"Verbose mode";
  "-index", Arg.Unit (fun _ -> action := Index) ,"Index a list of files";
  "-update", Arg.Unit (fun _ -> action := Update) ,"Replay last indexing command";
  "-locate", Arg.String (fun s -> action := Locate (split s)) ,"Locate an object or a package";
  "-complete", Arg.String (fun s -> action := Search (split s)) ,"Search for objects or packages matching a prefix";
  "-print", Arg.String (fun s -> action := Print (split s)) ,"Print the content of a package";
  "-only-ads", Arg.Set check_ext ,"Only index .abs files";
  "-scope", Arg.String (fun s -> scope := split s) ,"Set current scope";
]

let _ =
  try
    Arg.parse args (fun fn -> files_to_index := fn :: !files_to_index) "adaquery [options] -p myproject -index files\nadaquery [options] -p myproject -update\nadaquery [options] -p myproject (-locate|-search|-print) decl";
    begin match !action with
      | Index ->
        let tbl = Table.create () in
        List.iter (index_file tbl) !files_to_index;
        Table.write tbl !files_to_index !check_ext (Files.get_cache_file !project)
      | Locate s ->
        let (tbl,_,_) = Table.read (Files.get_cache_file !project) in
        let lst = Table.locate tbl !scope s in
        let print_loc lc =
          Printf.printf "%s\n%i\n" lc.Lexing.pos_fname lc.Lexing.pos_lnum
        in
        List.iter print_loc lst
      | Search s ->
        let (tbl,_,_) = Table.read (Files.get_cache_file !project) in
        let lst = Table.complete tbl !scope s in
        List.iter print_endline lst
      | Print s ->
        let (tbl,_,_) = Table.read (Files.get_cache_file !project) in
        Table.print tbl !scope s
      | Update ->
        let cache = Files.get_cache_file !project in
        let (_,files,opt_check_ext) = Table.read cache in
        let () = check_ext := opt_check_ext in
        let tbl = Table.create () in
        List.iter (index_file tbl) files;
        Table.write tbl files opt_check_ext cache
    end
  with
  | Sys_error err   -> Print.fail "Error %s." err
