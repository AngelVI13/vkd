open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type resultStatus = Finished | Dsq
[@@deriving yojson, eq, show { with_path = false }]

type t = {
  number : int;
  name : string;
  club : string;
  start : int;
  status : resultStatus;
  time : int;
  splits : Splits.t;
  stats : Runner_stats.t;
}
[@@deriving fields ~fields ~iterators:create, yojson, show]

let of_resp (runner : Response.RunnerResp.t) =
  let splits = Splits.of_string ~start:runner.start runner.splits in
  let finish = List.last_exn splits in
  (* `value_exn` here should be safe because everyone should have a finish time (i think?) *)
  let time = Option.value_exn finish.overall_time in

  Fields.create ~number:runner.number ~name:runner.name ~club:runner.club
    ~start:runner.start ~time
    ~status:(if runner.flag = 0 then Finished else Dsq)
    ~splits ~stats:(Runner_stats.empty ())

let print (r : t) (i : int) : unit =
  let stats_str = Runner_stats.yojson_of_t r.stats |> Yojson.Safe.to_string in
  printf "%d. %s: %s\n\n" (i + 1) r.name stats_str

let update_position_for_split r ~field ~split_idx ~position =
  let stats =
    if String.(Field.name field = Field.name Split.Fields.position) then
      Runner_stats.add_split_position_to_stats r.stats position
    else r.stats
  in

  let splits =
    List.mapi r.splits ~f:(fun i split ->
        if i = split_idx then Field.fset field split (Some position) else split)
  in
  { r with splits; stats }

let update_mistake_for_split r ~split_idx ~mistake_time =
  let stats =
    Runner_stats.add_mistake_to_stats r.stats ~mistake:mistake_time ~split_idx
  in
  let mistake_field = Split.Fields.mistake_time in
  let splits =
    List.mapi r.splits ~f:(fun i split ->
        if i = split_idx then Field.fset mistake_field split (Some mistake_time)
        else split)
  in
  { r with splits; stats }

let update_mistake_ratios r =
  let stats = Runner_stats.update_mistake_ratios r.stats in
  { r with stats }

let update_potential_time r =
  let stats =
    Field.fset Runner_stats.Fields.potential_time r.stats
      (Some (r.time - r.stats.mistake_time))
  in
  { r with stats }

let update_pvb_ratio r ~ratio =
  { r with stats = Runner_stats.update_pvb_ratio r.stats ratio }

let update_overall_position r ~field position =
  {
    r with
    stats = Runner_stats.add_overall_position_to_stats r.stats ~field position;
  }

let update_potential_position r position =
  let stats =
    Field.fset Runner_stats.Fields.potential_position r.stats (Some position)
    |> Runner_stats.update_mistakes_impact
  in
  { r with stats }

type trend = Up | Down | Stay [@@deriving show { with_path = false }]

let position_trend pos_a pos_b =
  let diff = pos_b - pos_a in
  (* if position B > position A -> runner dropped down places 
     if position B < position A -> runner got up a few places 
     if diff is about the same +- 1 place then no change happened *)
  if abs diff <= 1 then Stay else if diff > 0 then Down else Up

(* NOTE: this should only be called on Finished runner *)
let update_race_execution_aux r =
  let num_controls = List.length r.splits in
  let bucket_size = num_controls / 3 in
  let begin_idx = bucket_size in
  let mid_idx = 2 * bucket_size in
  let end_idx = num_controls - 1 in

  let begin_pos = Splits.overall_position_exn r.splits begin_idx in
  let mid_pos = Splits.overall_position_exn r.splits mid_idx in
  let end_pos = Splits.overall_position_exn r.splits end_idx in

  let trend_a = position_trend begin_pos mid_pos in
  let trend_b = position_trend mid_pos end_pos in

  let race_execution =
    match (trend_a, trend_b) with
    | Down, Down -> Runner_stats.PositiveSplit
    | Down, Up -> Runner_stats.FadeAndKick
    | Down, Stay -> Runner_stats.FadeAndHold
    | Up, Up -> Runner_stats.NegativeSplit
    | Up, Down -> Runner_stats.KickAndFade
    | Up, Stay -> Runner_stats.KickAndHold
    | Stay, Down -> Runner_stats.LateFade (* TODO: should this be HoldAndFade *)
    | Stay, Up -> Runner_stats.LateKick (* TODO: should this be HoldAndKick *)
    | Stay, Stay -> Runner_stats.EvenSplit
  in

  let stats =
    Field.fset Runner_stats.Fields.race_execution r.stats (Some race_execution)
  in
  { r with stats }

let update_race_execution r =
  (* NOTE: this is different than the `flag` value, it is determined by the
     actual data in the splits. Sometimes runners which are classified as
     finished have missing splits. It happens when sometimes the SI doesn't
     record the control but the event organizer checks that the person has been
     there by GPS tracking and he restores him in the results.
     Some examples can be found here: https://dbsportas.lt/lt/mvarz/269/split/2
     *)
  if not (Splits.has_dsq r.splits) then update_race_execution_aux r else r

let update_mistake_cluster r =
  if r.stats.mistake_num = 0 then r
  else
    let num_controls = List.length r.splits in
    let bucket_size = num_controls / 3 in
    let begin_idx = bucket_size in
    let mid_idx = 2 * bucket_size in
    let end_idx = num_controls in

    let begin_mistakes, mid_mistakes, end_mistakes =
      List.fold r.stats.mistake_indexes ~init:(0, 0, 0)
        ~f:(fun (begin_m, mid_m, end_m) mistake_idx ->
          if 0 <= mistake_idx && mistake_idx < begin_idx then
            (begin_m + 1, mid_m, end_m)
          else if begin_idx <= mistake_idx && mistake_idx < mid_idx then
            (begin_m, mid_m + 1, end_m)
          else if mid_idx <= mistake_idx && mistake_idx < end_idx then
            (begin_m, mid_m, end_m + 1)
          else assert false)
    in

    let num_mistakes = r.stats.mistake_num in
    let mistake_cluster =
      if Utils.calculate_percent begin_mistakes num_mistakes > 50 then
        Runner_stats.BeginningCluster
      else if Utils.calculate_percent mid_mistakes num_mistakes > 50 then
        Runner_stats.MiddleCluster
      else if Utils.calculate_percent end_mistakes num_mistakes > 50 then
        Runner_stats.EndCluster
      else Runner_stats.Scattered
    in

    let stats =
      Field.fset Runner_stats.Fields.mistake_cluster r.stats
        (Some mistake_cluster)
    in
    { r with stats }

let update_gender_or_group_position r ~field position =
  let stats = Field.fset field r.stats (Some position) in
  { r with stats }
