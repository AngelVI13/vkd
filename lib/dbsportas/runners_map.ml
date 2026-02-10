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

  (* TODO: move this outside maybe ? *)
  let module PersonalVsBest = struct
    type t = {
      sum_personal : int;
      sum_best : int;
      split_list : (int * int) list; [@opaque]
    }
    [@@deriving show { with_path = false }]

    let empty () = { sum_personal = 0; sum_best = 0; split_list = [] }

    let process_split (i : int) (t : t) (split : Split.t) =
      match split.time with
      | None -> t
      | Some time ->
          let fastest_time = List.nth_exn fst_times i in
          let snd_fastest_time = List.nth_exn snd_times i in
          let best =
            if time = fastest_time then snd_fastest_time else fastest_time
          in

          {
            sum_personal = t.sum_personal + time;
            sum_best = t.sum_best + best;
            split_list = t.split_list @ [ (time, best) ];
          }

    let rec filter_outliers (t : t) =
      let start_sum_personal = t.sum_personal in

      (* 
           Algorithm steps:
             1. Go through each control split starting from the last control 
             2. For each control calculate your current overall ratio 
                (sum of all best times for all controls / sum of all your times for all controls)
             3. Multiply your time to this control by the ratio -> that number
                should give you exactly the best time for that control if you
                performed in line with your overall ratio (from step 2)
             4. If the result is bigger than the best time for control + 60 seconds then 
                this indicates that you made a substantial mistake so therefore remove this split 
                from the split list (done later, step 6) and also subtract your
                split time from your sum of splits and subtract the best time
                from the sum of all best times. 
             5. This means that for the next split your ratio is unaffected by
                that substantial mistake
             6. After you have done this for all controls, if there were any substantial mistakes 
                remove them from the split list, and then return the whole
                algorithm (with the updated sums for personal and best times. 
             7. Recursion stops if there aren't any substantial mistakes.
         *)
      let to_remove, new_t =
        List.foldi (List.rev t.split_list) ~init:([], t)
          ~f:(fun i (to_remove, new_t) (personal, best) ->
            let best_vs_personal_ratio =
              Float.of_int t.sum_best /. Float.of_int t.sum_personal
            in
            let estimated_best =
              Float.of_int personal *. best_vs_personal_ratio
            in
            let is_outlier =
              Float.(estimated_best >= Float.of_int best +. 60.)
            in
            let real_idx = List.length t.split_list - i - 1 in

            if is_outlier then
              ( real_idx :: to_remove,
                {
                  new_t with
                  sum_personal = new_t.sum_personal - personal;
                  sum_best = new_t.sum_best - best;
                } )
            else (to_remove, new_t))
      in

      let new_splits =
        List.filteri new_t.split_list ~f:(fun i _ ->
            not (List.exists to_remove ~f:(Int.equal i)))
      in
      let new_t = { new_t with split_list = new_splits } in

      if start_sum_personal = new_t.sum_personal then t
      else filter_outliers new_t

    (** calculate a runners speed vs the best speed achieved during the race. If
        the ratio is 0.8 then it means you were running 20% slower than the best
        runner.

        Important: this should be called after filter_outliers. This is to make
        sure any big mistakes are removed from your performance ratio. *)
    let ratio (t : t) =
      if t.sum_best > 0 then Float.(of_int t.sum_personal / of_int t.sum_best)
      else 1.
  end in
  let _ =
    Map.map t ~f:(fun runner ->
        let personal_vs_best_ratio =
          List.foldi runner.splits ~init:(PersonalVsBest.empty ())
            ~f:PersonalVsBest.process_split
          |> PersonalVsBest.filter_outliers |> PersonalVsBest.ratio
        in

        (* TODO: calculate and update runner mistakes *)
        printf "%s %d: %f\n" runner.name runner.number personal_vs_best_ratio;
        t)
  in
  t
