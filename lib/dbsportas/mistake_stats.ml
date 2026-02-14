open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = { time : int; num : int; time_ratio : int }
[@@deriving fields ~fields ~iterators:create, yojson]

let empty () = { time = 0; num = 0; time_ratio = 0 }
