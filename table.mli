open Datatypes
type toplevel_tree
val tname_to_path : t_name -> path option (*TODO move in datatype*)
val add_decl : toplevel_tree -> t_decl -> unit
val locate : toplevel_tree -> path -> loc list
val complete : toplevel_tree -> path -> string list
val print : toplevel_tree -> path -> unit
val write : toplevel_tree -> string -> unit
val create : unit -> toplevel_tree
val read : string -> toplevel_tree
