open Common
let split s = Str.split_delim (Str.regexp_string ".") s

(* Option *)

type t_action =
  | Index
  | Search of path
  | Locate of path
  | Print of path
  | Extract_Alias
  | Update

let action = ref Index
let files_to_index = ref []
let check_ext = ref false

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
                                    Lexing.pos_fname = fullname file; };
          parse tbl lb
        end
    end
  with Sys_error err -> Print.fail "Error: %s" err

(* Alias *)

let get_decl_list (file:string) : t_decl list =
  let input = open_in file in
  let lb = Lexing.from_channel input in
  let _ = lb.Lexing.lex_curr_p <-
      { lb.Lexing.lex_curr_p with Lexing.pos_fname = fullname file; } in
  try
    Parser.goal_symbol Lexer.token lb
  with
    | Parser.Error ->
      ( Print.debug_with_loc lb.Lexing.lex_curr_p
          "Parsing error: unexpected token '%s'." (Lexing.lexeme lb); [] )
    | Lexer.Error (loc,msg) ->
      ( Print.debug_with_loc lb.Lexing.lex_curr_p "Lexing error: %s." msg; [] )

(* Main *)

let args = [
  "-p", Arg.Set_string Common.project ,"Set the project to use";
  "-v", Arg.Set Print.verbose_mode ,"Verbose mode";
  "-index", Arg.Unit (fun _ -> action := Index) ,"Index a list of files";
  "-update", Arg.Unit (fun _ -> action := Update) ,"Replay last indexing command";
  "-locate", Arg.String (fun s -> action := Locate (split s)) ,"Locate an object or a package";
  "-search", Arg.String (fun s -> action := Search (split s)) ,"Search for objects or packages matching a prefix";
  "-print", Arg.String (fun s -> action := Print (split s)) ,"Print the content of a package";
  "-extract-alias", Arg.Unit (fun _ -> action := Extract_Alias) ,"Print the content of a package";
  "-only-ads", Arg.Set check_ext ,"Only index .abs files";
  "-current-file", Arg.String Alias.set_current_file ,"Only index .abs files";
]

let _ =
  try
    Arg.parse args (fun fn -> files_to_index := fn :: !files_to_index) "adaquery -p myproject -index files\nadaquery -p myproject (-locate|-search|-print) decl"; (*FIXME*)
    begin match !action with
      | Index ->
        let tbl = Table.create () in
        List.iter (index_file tbl) !files_to_index;
        Table.write tbl !files_to_index !check_ext (get_cache_file ())
      | Locate s ->
        let (tbl,_,_) = Table.read (get_cache_file ()) in
        let lst = Table.locate tbl s in
        let print_loc lc =
          Printf.printf "%s\n%i\n" lc.Lexing.pos_fname lc.Lexing.pos_lnum
        in
        List.iter print_loc lst
      | Search s ->
        let (tbl,_,_) = Table.read (get_cache_file ()) in
        let lst = Table.complete tbl s in
        List.iter print_endline lst
      | Print s ->
        let (tbl,_,_) = Table.read (get_cache_file ()) in
        Table.print tbl s
      | Extract_Alias ->
        let lst =
          List.map (fun fn -> let fn = fullname fn in (fn, get_decl_list fn))
            !files_to_index
        in
        Alias.update (get_alias_file ()) lst
      | Update ->
        let cache = get_cache_file () in
        let (_,files,opt_check_ext) = Table.read cache in
        let () = check_ext := opt_check_ext in
        let tbl = Table.create () in
        List.iter (index_file tbl) files;
        Table.write tbl files opt_check_ext cache
    end
  with
  | Sys_error err   -> Print.fail "ERROR %s." err
