open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module CourseResp = struct
  type t = { id : string; controls : string } [@@deriving yojson]
end

module RunnerResp = struct
  type t = {
    number : int;
    name : string;
    club : string;
    course_name : string; [@key "class"]
    course_id : int;
    start : int;
    flag : int;
    splits : string;
  }
  [@@deriving yojson]
end

module ResultsTableResp = struct
  type t = {
    err : bool;
    courses : CourseResp.t list;
    runners : RunnerResp.t list;
  }
  [@@deriving yojson]
end

module Split = struct
  type t = {
    (* TODO: calculate position by class ? i.e. you might be 5th overall but 1st from womens35 group etc. *)
    time : int option;
    position : int option;
    overall_time : int option;
    overall_position : int option;
    timestamp : int option;
  }
  [@@deriving yojson, fields ~fields ~iterators:create]

  let empty () =
    {
      time = None;
      position = None;
      overall_time = None;
      overall_position = None;
      timestamp = None;
    }

  let make ~time ~overall_time ~timestamp =
    { timestamp; overall_time; time; position = None; overall_position = None }

  (* let from_time time overall_time absolute_time = *)
  (*   (* TODO: should these be options or keep it as -1 ?? *) *)
  (*   { time; overall_time; position = -1; overall_position = -1 } *)
end

module Splits = struct
  type t = Split.t list [@@deriving yojson]

  (* TODO: finish this *)
  let of_string ~(start : int) s : t =
    let timestamps =
      String.split s ~on:'-'
      |> List.map ~f:(fun time ->
             let time = String.strip time in
             if String.(time = "") then None else Some (Int.of_string time))
    in
    let splits =
      List.mapi timestamps ~f:(fun idx time ->
          let prev_time =
            if idx = 0 then Some start else List.nth_exn timestamps (idx - 1)
          in
          let control_time =
            Option.bind prev_time ~f:(fun prev ->
                Option.bind time ~f:(fun t -> Some (t - prev)))
          in
          let overall_time, timestamp =
            match time with
            | None -> (None, None)
            | Some t -> (Some (t - start), Some t)
          in

          Split.make ~time:control_time ~overall_time ~timestamp)
    in
    splits
end

type resultStatus = Finished | Dsq [@@deriving yojson, eq]

module RunnerResult = struct
  type t = {
    number : int;
    name : string;
    club : string;
    start : int;
    status : resultStatus;
    time : int;
    splits : Splits.t;
  }
  [@@deriving fields ~fields ~iterators:create, yojson]

  let of_resp (runner : RunnerResp.t) =
    let splits = Splits.of_string ~start:runner.start runner.splits in
    let finish = List.last_exn splits in
    (* `value_exn` here should be safe because everyone should have a finish time (i think?) *)
    let time = Option.value_exn finish.overall_time in

    Fields.create ~number:runner.number ~name:runner.name ~club:runner.club
      ~start:runner.start ~time
      ~status:(if runner.flag = 0 then Finished else Dsq)
      ~splits

  let update_position_for_split r split_idx position =
    let splits =
      List.mapi r.splits ~f:(fun i split ->
          if i = split_idx then { split with position = Some position }
          else split)
    in
    { r with splits }
end

module RunnersMap = struct
  type t = RunnerResult.t Int.Map.t

  let empty = Int.Map.empty

  let of_runners (runners : RunnerResult.t list) : t =
    List.fold runners ~init:empty ~f:(fun map runner ->
        Map.set map ~key:runner.number ~data:runner)

  let to_runners t = Map.data t

  let update_runner_position_for_split control_idx position t (runner_num, _) =
    Map.update t runner_num ~f:(fun runner ->
        match runner with
        | None -> assert false
        | Some runner ->
            RunnerResult.update_position_for_split runner control_idx position)
end

module CourseResult = struct
  type t = {
    course_name : string;
    course_id : int;
    controls : string list;
    finished : RunnerResult.t list;
    dsq : RunnerResult.t list;
  }
  [@@deriving fields, yojson]

  (* TODO: should this be here ? *)
  let filter_and_sort_splits splits_for_control =
    List.filter splits_for_control ~f:(fun (_, value) -> Option.is_some value)
    |> List.map ~f:(fun (runner_num, value) ->
           (runner_num, Option.value_exn value))
    |> List.sort_and_group ~compare:(fun (_, value1) (_, value2) ->
           Int.compare value1 value2)

  let of_resp (runners : RunnerResp.t list) (course : CourseResp.t) =
    let course_id = Int.of_string course.id in
    let runners = List.filter runners ~f:(fun r -> r.course_id = course_id) in
    let course_name =
      match List.hd runners with Some r -> r.course_name | None -> ""
    in
    let controls = String.split ~on:'-' course.controls in

    let runners = List.map runners ~f:RunnerResult.of_resp in
    let runners_map = RunnersMap.of_runners runners in

    (* get list of splits for each runner *)
    let all_splits =
      List.map runners ~f:(fun r ->
          List.map r.splits ~f:(fun s -> (r.number, s.time)))
    in

    (* TODO: have to do the exact same but for the absolute time to get overall position for that control *)
    let sorted_splits =
      (* create a List where each element is a list of all runners times for
         that control idx *)
      List.transpose_exn all_splits
      (* Remove any runner split values which don't have a time (when a user
         miss punched) & then sort all times for each control *)
      |> List.map ~f:filter_and_sort_splits
    in

    (* TODO: this is very ugly ... fix it *)
    let runners_map =
      List.foldi sorted_splits ~init:runners_map
        ~f:(fun control_idx map splits_for_control ->
          let _, map =
            List.fold splits_for_control ~init:(1, map)
              ~f:(fun (position, map) splits_with_same_time ->
                let map =
                  List.fold splits_with_same_time ~init:map
                    ~f:
                      (RunnersMap.update_runner_position_for_split control_idx
                         position)
                in
                let position_idx =
                  position + List.length splits_with_same_time
                in
                (position_idx, map))
          in
          map)
    in

    let runners = RunnersMap.to_runners runners_map in
    (* TODO: runner results should have Splits.t instead of Split.t list *)

    (* TODO: update splits here (positions). Probably will have to create a
       hashtable so i can update the runner split based on the runner number
     and split idx *)

    let runners =
      List.sort runners ~compare:(fun r1 r2 -> Int.compare r1.time r2.time)
    in

    let finished =
      List.filter runners ~f:(fun r -> equal_resultStatus r.status Finished)
    in
    let dsq =
      List.filter runners ~f:(fun r -> equal_resultStatus r.status Dsq)
    in

    Fields.create ~course_name ~course_id ~controls ~finished ~dsq
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : ResultsTableResp.t) =
    let course_results =
      List.map resp.courses ~f:(CourseResult.of_resp resp.runners)
    in
    { course_results }
end

let parse_course_results_table json_chan =
  let json = Yojson.Safe.from_channel json_chan in
  ResultsTableResp.t_of_yojson json

let%expect_test "parse_course_results_table" =
  let filename = "/home/angel/Documents/ocaml/vkd/splits_resp.json" in
  let splits_resp = In_channel.create filename in
  let results_table_resp = parse_course_results_table splits_resp in
  let results_table = ResultsTable.of_resp results_table_resp in
  (* let out = Yojson.Safe.to_string (ResultsTable.yojson_of_t results_table) in *)
  let out =
    List.nth_exn results_table.course_results 0
    |> CourseResult.yojson_of_t |> Yojson.Safe.to_string
  in
  (* Out_channel.write_all *)
  (*   "/home/angel/Documents/ocaml/vkd/course_results_test.json" ~data:out; *)
  (* Yojson.Safe.to_file *)
  (*   "/home/angel/Documents/ocaml/vkd/splits_resp_processed.json" *)
  (*   (ResultsTable.yojson_of_t results_table); *)
  printf "%s" out;
  [%expect
    {| {"course_results":[{"course_name":"1,2","course_id":"426148","controls":["49","31","32","42","41","45","47","35","40","36","44","33","37","46","30","43","39","48","38","34","69","FIN"],"results":[]},{"course_name":"3","course_id":"426149","controls":["31","44","42","36","45","40","41","33","37","49","39","48","46","38","34","69","FIN"],"results":[]},{"course_name":"4","course_id":"426150","controls":["37","32","44","33","49","46","38","30","43","48","34","69","FIN"],"results":[]},{"course_name":"D","course_id":"426152","controls":["51","52","60","54","55","59","57","58","37","34","56","46","48","69","FIN"],"results":[]},{"course_name":"P","course_id":"426151","controls":["37","31","49","48","46","56","34","69","FIN"],"results":[]}]} |}]
