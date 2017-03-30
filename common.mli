val project : string ref

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

val tname_to_path : t_name -> path option

val fullname : string -> string
val get_cache_dir  : unit -> string
val get_cache_file : unit -> string
val get_alias_file : unit -> string
