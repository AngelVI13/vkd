open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type resultStatus = Finished | Dsq [@@deriving yojson, eq, show]

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
  (* this is different than the `flag` value, it is determined by the actual data in the splits *)
  let has_dsq = Splits.has_dsq splits in

  Fields.create ~number:runner.number ~name:runner.name ~club:runner.club
    ~start:runner.start ~time
    ~status:(if runner.flag = 0 && not has_dsq then Finished else Dsq)
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

(* NOTE: this should only be called on Finished runner *)
let update_race_execution r =
  let num_controls = List.length r.splits in
  let middle_control_idx = num_controls / 2 in
  let middle_split = List.nth_exn r.splits middle_control_idx in
  let middle_position = Option.value_exn middle_split.overall_position in

  let end_split = List.last_exn r.splits in
  let end_position = Option.value_exn end_split.overall_position in

  let diff = end_position - middle_position in

  let race_execution =
    if Int.abs diff <= 2 then Runner_stats.EvenSplit
    else if diff > 0 then Runner_stats.PositiveSplit
    else Runner_stats.NegativeSplit
  in
  let stats =
    Field.fset Runner_stats.Fields.race_execution r.stats (Some race_execution)
  in
  { r with stats }

let update_mistake_cluster r =
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
