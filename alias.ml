open Common

type t = string*path

let get_alias (d:t_decl) : t list =
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

type alias_table = (string,t list) Hashtbl.t

let read_table prj : alias_table =
  let chn = open_in_bin prj in
  Marshal.from_channel chn

let write_table prj alias_table : unit =
  let chn = open_out_bin prj in
  Marshal.to_channel chn alias_table []

let update prj alias_list =
  let tb = read_table prj in
  let _ = List.iter (fun (fn,lst) -> Hashtbl.replace tb fn lst) alias_list in
  write_table prj tb
