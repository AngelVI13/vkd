open Core

type t = Victory | Defeat | Draw [@@deriving show { with_path = false }]

let t_of_int t =
  if Float.(t = 0.) then Defeat
  else if Float.(t = 1.) then Victory
  else if Float.(t = 0.5) then Draw
  else assert false

let int_of_t = function Victory -> 1. | Defeat -> 0. | Draw -> 0.5
