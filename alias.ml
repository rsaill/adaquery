open Common

let current_file = ref None

let set_current_file fn =
  current_file := Some (fullname fn)

let alias_from_decl (d:t_decl) : (string*path) list =
  let rec aux lst = function
  | Package (name,_,_,New tn)
  | Package (name,_,_,Renamed tn) ->
    begin match tname_to_path tn with
      | None -> lst
      | Some p -> (name,p)::lst
    end
  | Package (n_pkg,_,_,Decl dlst) -> List.fold_left aux lst dlst
  | Subprog _
  | Type _
  | Number _
  | Object _
  | Exception _
  | Other _ -> lst
  in
  aux [] d

type alias_table = (string,(string*path) list) Hashtbl.t

let read_table (alias_file:string) : alias_table =
  if Sys.file_exists alias_file then
    let chn = open_in_bin alias_file in
    Marshal.from_channel chn
  else
    Hashtbl.create 7

let write_table (alias_file:string) (alias_table:alias_table) : unit =
  let chn = open_out_bin alias_file in (*FIXME*)
  Marshal.to_channel chn alias_table []

let update (alias_file:string) (lst:(string*t_decl list) list) : unit =
  let tb = read_table alias_file in
  let _ = List.map (
      fun (filename,decl_lst) ->
        Hashtbl.replace tb filename (List.flatten (List.map alias_from_decl decl_lst))
    ) lst in
  write_table alias_file tb

let get_local_alias (name:string) : path option =
  match !current_file with
  | None -> None
  | Some fn ->
    let tbl = read_table (Common.get_alias_file ()) in
    try
      let lst = Hashtbl.find tbl fn in
      Some (List.assoc name lst)
    with
      Not_found -> None
