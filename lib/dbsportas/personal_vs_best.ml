open Core

type t = {
  sum_personal : int;
  sum_best : int;
  split_list : (int * int) list; [@opaque]
}
[@@deriving show { with_path = false }]

let empty () = { sum_personal = 0; sum_best = 0; split_list = [] }

let process_split ~fst_times ~snd_times (i : int) (t : t) (split : Split.t) =
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
        let estimated_best = Float.of_int personal *. best_vs_personal_ratio in
        let is_outlier = Float.(estimated_best >= Float.of_int best +. 60.) in
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

  if start_sum_personal = new_t.sum_personal then t else filter_outliers new_t

(** calculate a runners speed vs the best speed achieved during the race. If the
    ratio is 0.8 then it means you were running 20% slower than the best runner.

    Important: this should be called after filter_outliers. This is to make sure
    any big mistakes are removed from your performance ratio. *)
let ratio (t : t) =
  if t.sum_best > 0 then Float.(of_int t.sum_personal / of_int t.sum_best)
  else 1.
