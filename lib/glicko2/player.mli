type t

val show: t -> string
val pp: Format.formatter -> t -> unit

val create: rating:float -> rd:float -> vol:float -> tau:float -> default_rating:float -> id:int -> t

val rating: t -> float
val set_rating: t -> rating:float -> t

val rd: t -> float
val set_rd: t -> rd:float -> t

val vol: t -> float
val set_vol: t -> vol:float -> t

val add_result: t -> opponent:t -> outcome:Outcome.t -> t

val update_rank: t -> t

val predict: t -> opponent:t -> Outcome.t
