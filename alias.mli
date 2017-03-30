open Common

type t = string*path

val get_alias: t_decl -> t list
val update: string -> (string*t list) list -> unit
