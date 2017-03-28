type loc = Lexing.position
type path = string list (* A::B::C = package A.B.C *)
type ident = loc*string
type compound_name = ident list
type t_designator = Compound_Name of compound_name | String_Name of ident
type t_other = Subunit | Body_stub | Prot | Task
type t_name = Simple_name of ident | No_name | Selected_comp of t_name*ident

type t_decl =
  | Package of string * (loc*bool) * bool * t_package
  | Subprog of t_designator
  | Type of ident * (ident list) option
  | Number of ident list
  | Object of ident list
  | Exception of ident list
  | Other of t_other

and t_package = New of t_name | Renamed of t_name | Decl of t_decl list

let tname_to_path (tname:t_name) : path option =
  let rec aux : t_name -> path = function
  | Simple_name id -> [snd id]
  | No_name -> raise (Failure "No_name")
  | Selected_comp (tname,id) -> (snd id)::(aux tname)
  in
  try Some (List.rev (aux tname))
  with Failure _ -> None