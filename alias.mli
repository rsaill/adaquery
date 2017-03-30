open Common

val set_current_file : string -> unit

val get_local_alias: string -> path option

val update: string(*alias_cache_file*) -> (string(*filename*) * t_decl list) list -> unit
