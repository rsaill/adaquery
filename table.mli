open Common
type toplevel_tree
val add_decl : toplevel_tree -> t_decl -> unit
val locate : toplevel_tree -> path -> loc list
val complete : toplevel_tree -> path -> string list
val print : toplevel_tree -> path -> unit
val write : toplevel_tree -> string list -> bool -> string -> unit
val create : unit -> toplevel_tree
val read : string -> toplevel_tree * string list * bool
