open Core

type t = Runner_result.t Int.Map.t

let empty = Int.Map.empty

let of_runners (runners : Runner_result.t list) : t =
  List.fold runners ~init:empty ~f:(fun map runner ->
      Map.set map ~key:runner.number ~data:runner)

let to_runners t = Map.data t

let update_runner t ~runner_num ~f =
  Map.update t runner_num ~f:(fun runner ->
      match runner with None -> assert false | Some runner -> f runner)

let filter_and_sort_splits splits_for_control =
  List.filter splits_for_control ~f:(fun (_, value) -> Option.is_some value)
  |> List.map ~f:(fun (runner_num, value) ->
         (runner_num, Option.value_exn value))
  |> List.sort_and_group ~compare:(fun (_, value1) (_, value2) ->
         Int.compare value1 value2)

let update_runner_positions_for_same_time ~field ~control_idx (position, map)
    splits_with_same_time =
  let map =
    List.fold splits_with_same_time ~init:map ~f:(fun map (runner_num, _) ->
        update_runner map ~runner_num
          ~f:
            (Runner_result.update_position_for_split ~field
               ~split_idx:control_idx ~position))
  in
  let position_idx = position + List.length splits_with_same_time in
  (position_idx, map)

(* get list of sublists. Each sublist represent each runners time for that
     control idx. Sublists are sorted from fastest to slowest. Each sublist
     element is another list because sometimes multiple runners have the same
     time
     ~time_field: specifies which field to sorting splits on, individual control time, overall time etc.
     *)
let all_sorted_splits ~time_field (runners : Runner_result.t list) =
  (* get list of splits for each runner *)
  let all_splits =
    List.map runners ~f:(fun r ->
        List.map r.splits ~f:(fun s -> (r.number, Field.get time_field s)))
  in

  let sorted_splits =
    (* create a List where each element is a list of all runners times for
         that control idx *)
    List.transpose_exn all_splits
    (* Remove any runner split values which don't have a time (when a user
         miss punched) & then sort all times for each control *)
    |> List.map ~f:filter_and_sort_splits
  in
  sorted_splits

(* get list of first best & list of second best splits for each control *)
let fst_and_snd_best_splits ~time_field (t : t) =
  let sorted_splits = all_sorted_splits ~time_field (Map.data t) in
  let fst_and_snd_splits =
    List.map sorted_splits ~f:(fun splits_for_control ->
        let fst = List.hd_exn splits_for_control in
        let snd =
          match List.nth splits_for_control 1 with
          | None -> fst
          | Some snd -> snd
        in

        let _, fst = List.hd_exn fst in
        let _, snd = List.hd_exn snd in
        [ fst; snd ])
    |> List.transpose_exn
  in

  let fst_times = List.nth_exn fst_and_snd_splits 0 in
  let snd_times = List.nth_exn fst_and_snd_splits 1 in

  (fst_times, snd_times)

let update_runner_positions ~time_field ~position_field (t : t) =
  let sorted_splits = all_sorted_splits ~time_field (Map.data t) in
  (* update runners position in map *)
  List.foldi sorted_splits ~init:t ~f:(fun control_idx map splits_for_control ->
      let update_fn =
        update_runner_positions_for_same_time ~field:position_field ~control_idx
      in
      (* position num starts from 1, here splits_for_control contains
           sublists of all runners with the same time *)
      let _, map = List.fold splits_for_control ~init:(1, map) ~f:update_fn in
      map)

let calculate_mistake ~(time : int) ~(reference : int) ~(perf_ratio : float) =
  Float.(to_int (round_nearest (of_int time - (of_int reference * perf_ratio))))

let calculate_pvb_ratio ~fst_times ~snd_times splits =
  List.foldi splits
    ~init:(Personal_vs_best.empty ())
    ~f:(Personal_vs_best.process_split ~fst_times ~snd_times)
  |> Personal_vs_best.filter_outliers |> Personal_vs_best.ratio

let update_runner_mistake_splits (t : t) ~fst_times ~snd_times
    (runner : Runner_result.t) : t =
  let runner_num = runner.number in
  let personal_vs_best_ratio =
    calculate_pvb_ratio ~fst_times ~snd_times runner.splits
  in

  (* printf "%s %d: %f [" runner.name runner_num personal_vs_best_ratio; *)
  let new_t =
    List.foldi runner.splits ~init:t ~f:(fun control_idx t split ->
        match split.time with
        | None -> t
        | Some time ->
            let best_time = List.nth_exn fst_times control_idx in
            let snd_time = List.nth_exn snd_times control_idx in
            let reference = if time > best_time then best_time else snd_time in
            let mistake =
              calculate_mistake ~time ~reference
                ~perf_ratio:personal_vs_best_ratio
            in

            if mistake >= 10 then
              update_runner t ~runner_num
                ~f:
                  (Runner_result.update_mistake_for_split ~split_idx:control_idx
                     ~mistake_time:mistake)
            else t)
  in
  let new_t =
    update_runner new_t ~runner_num ~f:Runner_result.update_mistake_ratios
    |> update_runner ~runner_num ~f:Runner_result.update_potential_time
    |> update_runner ~runner_num
         ~f:(Runner_result.update_pvb_ratio ~ratio:personal_vs_best_ratio)
  in
  (* printf "]\n"; *)
  new_t

let update_runner_mistakes (t : t) : t =
  let times =
    try Some (fst_and_snd_best_splits ~time_field:Split.Fields.time t)
    with _ -> None
  in

  match times with
  | None -> t
  | Some (fst_times, snd_times) ->
      if
        (* TODO: if we have just 1 runner, we don't calculate mistakes. TEST THIS !!!! *)
        List.hd_exn fst_times = List.hd_exn snd_times
      then t
      else
        List.fold (Map.data t) ~init:t
          ~f:(update_runner_mistake_splits ~fst_times ~snd_times)
