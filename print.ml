open Datatypes

let rec pp_cname out (cn:compound_name) : unit =
  Printf.fprintf out "%s" (String.concat "." (List.map snd cn))

let pp_designator out : t_designator -> unit = function
  | Compound_Name cname -> pp_cname out cname
  | String_Name str -> Printf.fprintf out "%s" (snd str)

let pp_other out : t_other -> unit = function
  | Subunit -> Printf.fprintf out "Subunit"
  | Body_stub -> Printf.fprintf out "Body_stub"
  | Prot -> Printf.fprintf out "Prototype"
  | Task -> Printf.fprintf out "Task"

let rec pp_name out : t_name -> unit = function
  | Simple_name s -> Printf.fprintf out "%s" (snd s)
  | No_name -> Printf.fprintf out "???"
  | Selected_comp (n,s) -> ( pp_name out n; Printf.fprintf out ".%s" (snd s) )

let its indent = String.make indent ' '

let rec print (indent:int) : t_decl -> unit = function
  | Package (pname,_,_,Decl lst) ->
    begin
      Printf.printf "%s[Package] %s.\n" (its indent) pname;
      List.iter (print (indent + 3)) lst
    end
  | Package (pname,_,_,New name) ->
    Printf.printf "%s[Package] %s = new %a.\n" (its indent) pname pp_name name
  | Package (pname,_,_,Renamed name) ->
    Printf.printf "%s[Package] %s renames %a.\n" (its indent) pname pp_name name
  | Subprog d -> Printf.printf "%s[Subprogram] %a.\n" (its indent) pp_designator d
  | Type (name,None) -> Printf.printf "%s[Type] %s.\n" (its indent) (snd name)
  | Type (name,Some lst) ->
    Printf.printf "%s[Type] %s = (%s).\n" (its indent) (snd name) (String.concat ", " (List.map snd lst))
  | Number lst ->
    Printf.printf "%s[Number] %s.\n" (its indent) (String.concat ", " (List.map snd lst))
  | Object lst ->
    Printf.printf "%s[Object] %s.\n" (its indent) (String.concat ", " (List.map snd lst))
  | Exception lst ->
    Printf.printf "%s[Exception] %s.\n" (its indent) (String.concat ", " (List.map snd lst))
  | Other o -> Printf.printf "%s[Other] %a.\n" (its indent) pp_other o
