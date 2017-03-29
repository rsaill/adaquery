open Common
let split s = Str.split_delim (Str.regexp_string ".") s

(* Option *)

type t_action =
  | Index
  | Search of path
  | Locate of path
  | Print of path
  | Extract_Alias

let action = ref Index
let project = ref "myproject" 
let files_to_index = ref []

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

let rec index_file (tbl:Table.toplevel_tree) (file:string) : unit =
  try
    begin
      if Sys.is_directory file then
        Array.iter (index_file tbl) (Sys.readdir file)
      else
        let input = open_in file in
        let lb = Lexing.from_channel input in
        lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with
                                  Lexing.pos_fname = Files.fullname file; };
        parse tbl lb
    end
  with Sys_error err -> Print.fail "Error: %s" err

(* Alias *)

let extract_alias (file:string) : (string*path) list =
  let input = open_in file in
  let lb = Lexing.from_channel input in
  let _ = lb.Lexing.lex_curr_p <-
      { lb.Lexing.lex_curr_p with Lexing.pos_fname = Files.fullname file; } in
  try
    List.flatten (List.map Alias.get_alias (Parser.goal_symbol Lexer.token lb))
  with
    | Parser.Error ->
      ( Print.debug_with_loc lb.Lexing.lex_curr_p
          "Parsing error: unexpected token '%s'." (Lexing.lexeme lb); [] )
    | Lexer.Error (loc,msg) ->
      ( Print.debug_with_loc lb.Lexing.lex_curr_p "Lexing error: %s." msg; [] )

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
        Table.write tbl (Files.get_cache_file !project)
      | Locate s ->
        let tbl = Table.read (Files.get_cache_file !project) in
        let lst = Table.locate tbl s in
        let print_loc lc =
          Printf.printf "%s\n%i\n" lc.Lexing.pos_fname lc.Lexing.pos_lnum
        in
        List.iter print_loc lst
      | Search s ->
        let tbl = Table.read (Files.get_cache_file !project) in
        let lst = Table.complete tbl s in
        List.iter print_endline lst
      | Print s ->
        let tbl = Table.read (Files.get_cache_file !project) in
        Table.print tbl s
      | Extract_Alias ->
        let alias =
          List.map (fun fn -> let fn = Files.fullname fn in (fn, extract_alias fn))
            !files_to_index
        in
        Alias.update (Files.get_alias_file !project) alias
    end
  with
  | Sys_error err   -> Print.fail "ERROR %s." err
