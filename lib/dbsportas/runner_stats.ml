open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  mistake_time : int;
  mistake_num : int;
  mistake_indexes : int list;
  (* mistake < 60 seconds *)
  small_mistakes : Mistake_stats.t;
  (* mistake < 120 seconds *)
  big_mistakes : Mistake_stats.t;
  (* mistake >= 120 seconds *)
  blunder_mistakes : Mistake_stats.t;
  (* tilt rate refers to amount of mistakes after which you make another
     mistake / divided by the total number of mistakes

    For example: if you made 4 mistakes total: 1st 5th 7th and 12th control - the tilt rate is 0%
    but if you made 2 mistakes: 1st and 2nd control - the tilt rate would be 50% (meaning that half 
    the times you make a mistake you are likely to make another one straight after

    if you made 4 mistakes - 1st, 2nd and 5th and 7th - 25% tilt rate
    if you made 4 mistakes - 1st, 2nd and 5th and 6th - 50% tilt rate 
    if you made 4 mistakes - 1st, 2nd, 3rd, 4th - 75% tilt rate
     *)
  consecutive_mistakes : int;
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
  (* removing any big mistakes, this takes your times vs the best times for each split.
   I.e. if your ratio is 50% this means that you ran 2x slower than the best times for 
   each control *)
  performance : int;
  overall_position : int option;
  (* M(oteris) / V(yras) *)
  position_gender : int option;
  (* M-21A; V-12; M-D40; V-D21 *)
  position_group : int option;
      (* calculate potential time, this is time - all mistakes 
and calculate potential position i.e. if you didn't have any mistakes then what position would you be .
maybe also calculate if nobody did any mistakes then what position would you take
   *)

      (* calculate how evenly you ran the race. Take average of all your
         positions for splits where you didn't make a mistake, then split the
         controls to first half and second half (or into 3) and try to identify
         if you overpushed in the beginning or you ran the race evenly etc.


         or maybe a better way to calculate it is to split the race into 3
         parts and then sum your time for each part. then compare with the
         people finished next to you and determine if you lost time compared to
         them or if you gained compare to them etc.
         *)
}
[@@deriving fields ~fields ~iterators:create, yojson]

let empty () =
  {
    mistake_time = 0;
    mistake_num = 0;
    mistake_indexes = [];
    small_mistakes = Mistake_stats.empty ();
    big_mistakes = Mistake_stats.empty ();
    blunder_mistakes = Mistake_stats.empty ();
    consecutive_mistakes = 0;
    tilt_rate = 0;
    flow_rate = 0;
    best_splits = 0;
    top5_splits = 0;
    top10_splits = 0;
    performance = 0;
    overall_position = None;
    position_gender = None;
    position_group = None;
  }

let update_field t ~field ~f = Field.fset field t (f (Field.get field t))
let incr_field t ~field = update_field t ~field ~f:(fun v -> v + 1)

let add_split_position_to_stats t position =
  let field1 = if position = 1 then Some Fields.best_splits else None in
  let field2 = if position <= 5 then Some Fields.top5_splits else None in
  let field3 = if position <= 10 then Some Fields.top10_splits else None in
  let fields_to_update = [ field1; field2; field3 ] |> List.filter_opt in

  List.fold fields_to_update ~init:t ~f:(fun t field -> incr_field t ~field)

let add_overall_position_to_stats t ~field position =
  Field.fset field t (Some position)

let add_mistake_to_stats t ~mistake ~split_idx =
  let new_mistake_time = t.mistake_time + mistake in
  let new_mistake_num = t.mistake_num + 1 in
  let new_mistake_indexes = split_idx :: t.mistake_indexes in

  let new_consecutive_mistakes =
    (* If we have a mistake right before this one, increment consecutive_mistakes *)
    if List.count t.mistake_indexes ~f:(Int.equal (split_idx - 1)) > 0 then
      t.consecutive_mistakes + 1
    else t.consecutive_mistakes
  in
  let new_tilt_rate =
    Utils.calculate_percent new_consecutive_mistakes new_mistake_num
  in

  let mistake_field =
    if mistake <= 30 then Fields.small_mistakes
    else if mistake <= 120 then Fields.big_mistakes
    else Fields.blunder_mistakes
  in
  let mistake_t = Field.get mistake_field t in
  let mistake_t = Mistake_stats.update_time mistake_t mistake in
  let new_t = Field.fset mistake_field t mistake_t in
  {
    new_t with
    mistake_time = new_mistake_time;
    mistake_num = new_mistake_num;
    mistake_indexes = new_mistake_indexes;
    consecutive_mistakes = new_consecutive_mistakes;
    tilt_rate = new_tilt_rate;
  }

let update_mistake_ratios t =
  [ Fields.small_mistakes; Fields.big_mistakes; Fields.blunder_mistakes ]
  |> List.fold ~init:t ~f:(fun t field ->
         update_field t ~field
           ~f:
             (Mistake_stats.update_ratio ~overall_time:t.mistake_time
                ~overall_num:t.mistake_num))

let update_pvb_ratio t ratio =
  (* NOTE: the ratio here is in the form 1.1567 - which means you took 15% more
     time on average to each control compared to the best times for that
     control. Because of that we convert it to the more human readable form of
     percentage from the best times, i.e. your performance is at 85% of the best *)
  let performance = 1. /. ratio *. 100.0 in
  let performance = Float.(to_int (round_nearest performance)) in
  { t with performance }
