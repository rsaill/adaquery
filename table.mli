type t
val add_decl : t -> Datatypes.t_decl -> unit
val locate : t -> Datatypes.path -> Datatypes.loc list
val search : t -> Datatypes.path -> string list
val print : t -> Datatypes.path -> unit
val write : t -> string -> unit
val create : unit -> t
val read : string -> t
