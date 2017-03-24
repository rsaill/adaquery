open Datatypes
type t
val tname_to_path : t_name -> path option (*TODO move in datatype*)
val add_decl : t -> t_decl -> unit
val locate : t -> path -> loc list
val search : t -> path -> string list
val print : t -> path -> unit
val write : t -> string -> unit
val create : unit -> t
val read : string -> t
