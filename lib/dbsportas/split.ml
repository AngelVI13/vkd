open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  (* TODO: calculate position by class ? i.e. you might be 5th overall but 1st from womens35 group etc. *)
  time : int option;
  position : int option;
  overall_time : int option;
  overall_position : int option;
  timestamp : int option;
}
[@@deriving yojson, fields ~fields ~iterators:create]

let empty () =
  {
    time = None;
    position = None;
    overall_time = None;
    overall_position = None;
    timestamp = None;
  }

let make ~time ~overall_time ~timestamp =
  { timestamp; overall_time; time; position = None; overall_position = None }

(* let from_time time overall_time absolute_time = *)
(*   (* TODO: should these be options or keep it as -1 ?? *) *)
(*   { time; overall_time; position = -1; overall_position = -1 } *)
