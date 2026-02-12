open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  mistake_time : int;
  (* mistake < 60 seconds *)
  small_mistakes_time : int;
  small_mistakes_num : int;
  small_mistakes_time_ratio : int;
  (* mistake < 120 seconds *)
  big_mistakes_time : int;
  big_mistakes_num : int;
  big_mistakes_time_ratio : int;
  (* mistake >= 120 seconds *)
  blunders_time : int;
  blunders_num : int;
  blunders_time_ratio : int;
  (* tilt rate refers to amount of mistakes after which you make another
     mistake / divided by the total number of mistakes

    For example: if you made 4 mistakes total: 1st 5th 7th and 12th control - the tilt rate is 0%
    but if you made 2 mistakes: 1st and 2nd control - the tilt rate would be 50% (meaning that half 
    the times you make a mistake you are likely to make another one straight after

    if you made 4 mistakes - 1st, 2nd and 5th and 7th - 25% tilt rate
    if you made 4 mistakes - 1st, 2nd and 5th and 6th - 50% tilt rate 
    if you made 4 mistakes - 1st, 2nd, 3rd, 4th - 75% tilt rate
     *)
  tilt_rate : int;
  (* flow rate refers to the amount of good control times in a row 
     For example if you had the following split times: 4th; 5th; 3rd; 5th; 4th etc. -> you have 100% flow rate
     TODO: add more examples here
     *)
  flow_rate : int;
  (* 1st place time for the split *)
  best_splits : int;
  top5_splits : int;
  top10_splits : int;
  overall_position : int option;
  (* M(oteris) / V(yras) *)
  position_gender : int option;
  (* M-21A; V-12; M-D40; V-D21 *)
  position_group : int option;
}
[@@deriving fields ~fields ~iterators:create, yojson]

let empty () =
  {
    mistake_time = 0;
    small_mistakes_time = 0;
    small_mistakes_num = 0;
    small_mistakes_time_ratio = 0;
    big_mistakes_time = 0;
    big_mistakes_num = 0;
    big_mistakes_time_ratio = 0;
    blunders_time = 0;
    blunders_num = 0;
    blunders_time_ratio = 0;
    tilt_rate = 0;
    best_splits = 0;
    top5_splits = 0;
    top10_splits = 0;
    overall_position = None;
    position_gender = None;
    position_group = None;
  }

let add_split_position_to_stats t position =
  let position_stat_field =
    if position = 1 then Some Fields.best_splits
    else if position <= 5 then Some Fields.top5_splits
    else if position <= 10 then Some Fields.top10_splits
    else None
  in
  match position_stat_field with
  | None -> t
  | Some field -> Field.fset field t (Field.get field t + 1)
