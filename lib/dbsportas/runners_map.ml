open Core

type t = Runner_result.t Int.Map.t

let empty = Int.Map.empty

let of_runners (runners : Runner_result.t list) : t =
  List.fold runners ~init:empty ~f:(fun map runner ->
      Map.set map ~key:runner.number ~data:runner)

let to_runners t = Map.data t

let update_runner_position_for_split ~field control_idx position t
    (runner_num, _) =
  Map.update t runner_num ~f:(fun runner ->
      match runner with
      | None -> assert false
      | Some runner ->
          Runner_result.update_position_for_split ~field runner control_idx
            position)

let filter_and_sort_splits splits_for_control =
  List.filter splits_for_control ~f:(fun (_, value) -> Option.is_some value)
  |> List.map ~f:(fun (runner_num, value) ->
         (runner_num, Option.value_exn value))
  |> List.sort_and_group ~compare:(fun (_, value1) (_, value2) ->
         Int.compare value1 value2)

let update_runner_positions_for_same_time ~field ~control_idx (position, map)
    splits_with_same_time =
  let map =
    List.fold splits_with_same_time ~init:map
      ~f:(update_runner_position_for_split ~field control_idx position)
  in
  let position_idx = position + List.length splits_with_same_time in
  (position_idx, map)

(* get list of sublists. Each sublist represent each runners time for that
     control idx. Sublists are sorted from fastest to slowest. Each sublist
     element is another list because sometimes multiple runners have the same
     time
     ~time_field: specifies which field to sorting splits on, individual control time, overall time etc.
     *)
let all_sorted_splits ~time_field (t : t) =
  let runners = Map.data t in
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
  let sorted_splits = all_sorted_splits ~time_field t in
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

  (* TODO: these should not be calculated again for every single runner cause they will be the same *)
  let fst_times = List.nth_exn fst_and_snd_splits 0 in
  let snd_times = List.nth_exn fst_and_snd_splits 1 in

  (fst_times, snd_times)

let update_runner_positions ~time_field ~position_field (t : t) =
  let sorted_splits = all_sorted_splits ~time_field t in
  (* update runners position in map *)
  List.foldi sorted_splits ~init:t ~f:(fun control_idx map splits_for_control ->
      let update_fn =
        update_runner_positions_for_same_time ~field:position_field ~control_idx
      in
      (* position num starts from 1, here splits_for_control contains
           sublists of all runners with the same time *)
      let _, map = List.fold splits_for_control ~init:(1, map) ~f:update_fn in
      map)

let update_runner_mistakes (t : t) : t =
  let fst_times, snd_times =
    fst_and_snd_best_splits ~time_field:Split.Fields.time t
  in

  let _ =
    Map.map t ~f:(fun runner ->
        let personal_vs_best_ratio =
          List.foldi runner.splits
            ~init:(Personal_vs_best.empty ())
            ~f:(Personal_vs_best.process_split ~fst_times ~snd_times)
          |> Personal_vs_best.filter_outliers |> Personal_vs_best.ratio
        in

        (* TODO: calculate and update runner mistakes *)
        printf "%s %d: %f\n" runner.name runner.number personal_vs_best_ratio;
        t)
  in
  t
