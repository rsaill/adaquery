open Common

val pp_cname : out_channel -> compound_name -> unit
val pp_designator : out_channel -> t_designator -> unit
val pp_other : out_channel -> t_other -> unit
val pp_name : out_channel -> t_name -> unit
val pp_path : out_channel -> path -> unit
val print : int -> t_decl -> unit
val verbose_mode : bool ref
val debug : ('a, out_channel, unit, unit) format4 -> 'a
val debug_with_loc : Lexing.position -> ('a, out_channel, unit, unit) format4 -> 'a
val fail : ('a, out_channel, unit, 'b) format4 -> 'a
