open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = { time : int; num : int; time_ratio : int }
[@@deriving fields ~fields ~iterators:create, yojson]

let empty () = { time = 0; num = 0; time_ratio = 0 }

let calc_percent part overall =
  Float.(to_int (round_nearest 100.0 * (of_int part / of_int overall)))

let update t time =
  let new_time = t.time + time in
  let new_num = t.num + 1 in
  { t with time = new_time; num = new_num }
