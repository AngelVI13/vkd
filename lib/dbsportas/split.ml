open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  time : int option;
  position : int option;
  position_gender : int option;
  position_group : int option;
  overall_time : int option;
  overall_position : int option;
  overall_position_gender : int option;
  overall_position_group : int option;
  timestamp : int option;
  mistake_time : int option;
}
[@@deriving yojson, fields ~fields ~iterators:create, show]

let empty () =
  {
    time = None;
    position = None;
    position_gender = None;
    position_group = None;
    overall_time = None;
    overall_position = None;
    overall_position_gender = None;
    overall_position_group = None;
    timestamp = None;
    mistake_time = None;
  }

let make ~time ~overall_time ~timestamp =
  {
    timestamp;
    overall_time;
    time;
    position = None;
    position_gender = None;
    position_group = None;
    overall_position = None;
    overall_position_gender = None;
    overall_position_group = None;
    mistake_time = None;
  }
