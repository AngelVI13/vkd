open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  (* TODO: calculate position by class ? i.e. you might be 5th overall but 1st from womens35 group etc. *)
  time : int option;
  position : int option;
  overall_time : int option;
  overall_position : int option;
  timestamp : int option;
  mistake_time : int option;
}
[@@deriving yojson, fields ~fields ~iterators:create, show]

let empty () =
  {
    time = None;
    position = None;
    overall_time = None;
    overall_position = None;
    timestamp = None;
    mistake_time = None;
  }

let make ~time ~overall_time ~timestamp =
  {
    timestamp;
    overall_time;
    time;
    position = None;
    overall_position = None;
    mistake_time = None;
  }
