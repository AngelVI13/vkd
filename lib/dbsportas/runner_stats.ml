open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  mistake_time : int;
  mistake_num : int;
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
    small_mistakes = Mistake_stats.empty ();
    big_mistakes = Mistake_stats.empty ();
    blunder_mistakes = Mistake_stats.empty ();
    tilt_rate = 0;
    flow_rate = 0;
    best_splits = 0;
    top5_splits = 0;
    top10_splits = 0;
    overall_position = None;
    position_gender = None;
    position_group = None;
  }

let add_split_position_to_stats t position =
  let position_stat_field =
    (* TODO: what makes sense to present this data? *)
    (* - 1st: 1 || top5: 8 || top10: 7 *)
    (* - 1st: 1 || top5: 9 || top10: 16 *)
    (* SHOULD the totals include the smaller subsection in itself or not? *)
    if position = 1 then Some Fields.best_splits
    else if position <= 5 then Some Fields.top5_splits
    else if position <= 10 then Some Fields.top10_splits
    else None
  in
  match position_stat_field with
  | None -> t
  | Some field -> Field.fset field t (Field.get field t + 1)

let add_mistake_to_stats t mistake =
  let new_mistake_time = t.mistake_time + mistake in
  let new_mistake_num = t.mistake_num + 1 in

  let mistake_field =
    if mistake < 60 then Fields.small_mistakes
    else if mistake < 120 then Fields.big_mistakes
    else Fields.blunder_mistakes
  in
  let mistake_t = Field.get mistake_field t in
  let mistake_t =
    Mistake_stats.update mistake_t ~time:mistake
      ~overall_mistake_time:new_mistake_time
  in
  let new_t = Field.fset mistake_field t mistake_t in
  { new_t with mistake_time = new_mistake_time; mistake_num = new_mistake_num }
