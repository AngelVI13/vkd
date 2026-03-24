open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module CourseResult = struct
  type t = {
    course_name : string;
    course_id : int;
    controls : string list;
    finished : Runner_result.t list;
    dsq : Runner_result.t list;
  }
  [@@deriving fields, yojson, show]

  let of_resp (runners : Response.RunnerResp.t list)
      (course : Response.CourseResp.t) =
    let course_id = Int.of_string course.id in
    let runners = List.filter runners ~f:(fun r -> r.course_id = course_id) in
    let course_name =
      match List.hd runners with Some r -> r.course_name | None -> ""
    in
    let controls = String.split ~on:'-' course.controls in

    let control_time_field = Split.Fields.time in
    let control_position_field = Split.Fields.position in
    let overall_time_field = Split.Fields.overall_time in
    let overall_position_field = Split.Fields.overall_position in

    let runners =
      (* create RunnerResult.t from json response *)
      List.map runners ~f:Runner_result.of_resp
      (* create RunnersMap.t and calculate positions (control & overall) for each runner *)
      |> Runners_map.of_runners
      |> Runners_map.update_runner_positions ~time_field:control_time_field
           ~position_field:control_position_field
      |> Runners_map.update_runner_positions ~time_field:overall_time_field
           ~position_field:overall_position_field
      |> Runners_map.update_runner_mistakes |> Runners_map.to_runners
      (* sort runners based on their overall time *)
      |> List.sort ~compare:(fun r1 r2 -> Int.compare r1.time r2.time)
    in

    let finished =
      List.filter runners ~f:(fun r ->
          Runner_result.equal_resultStatus r.status Runner_result.Finished)
      |> List.mapi ~f:(fun i r ->
             Runner_result.update_overall_position r
               ~field:Runner_stats.Fields.overall_position (i + 1)
             |> Runner_result.update_race_execution
             |> Runner_result.update_mistake_cluster)
    in

    (* calculate potential position by taking your potential time and finding
       your potential position by looping thourgh the results from top to bottom
       and finding the position at which a runners time is bigger than yours, that is
       your potential position *)
    let finished =
      List.map finished ~f:(fun r ->
          let potential_position =
            if Option.value_exn r.stats.overall_position = 1 then 1
            else
              let potential_time = Option.value_exn r.stats.potential_time in
              let position =
                List.fold finished ~init:None ~f:(fun pos r ->
                    match pos with
                    | Some _ -> pos
                    | None ->
                        if r.time > potential_time then r.stats.overall_position
                        else None)
                |> Option.value_exn
              in
              position
          in
          Runner_result.update_potential_position r potential_position)
    in

    let dsq =
      List.filter runners ~f:(fun r ->
          Runner_result.equal_resultStatus r.status Runner_result.Dsq)
    in

    Fields.create ~course_name ~course_id ~controls ~finished ~dsq

  let update_gender (t : t) (runner_map : String.t Int.Map.t)
      ~(gender_prefix : string) =
    let _, finished =
      List.fold t.finished ~init:(1, []) ~f:(fun (position, finished) r ->
          let r_group = Map.find_exn runner_map r.number in

          if String.is_prefix ~prefix:gender_prefix r_group then
            let r =
              Runner_result.update_gender_or_group_position r
                ~field:Runner_stats.Fields.position_gender position
            in
            (position + 1, r :: finished)
          else (position, r :: finished))
    in
    (* TODO: here create a RunnersMap and calculate the splits positions based on gender and later based on group *)

    (* here we reverse the finished because while `folding` we created the new
       list in reverse order *)
    { t with finished = List.rev finished }

  let update_group (t : t) (runner_map : String.t Int.Map.t) =
    let groups =
      List.sort_and_group t.finished ~compare:(fun r1 r2 ->
          let group1 = Map.find_exn runner_map r1.number in
          let group2 = Map.find_exn runner_map r2.number in
          String.compare group1 group2)
    in
    let group_pos_map : Int.t Int.Map.t = Int.Map.empty in
    let group_pos_map =
      List.fold groups ~init:group_pos_map ~f:(fun group_pos_map group ->
          let map =
            List.foldi group ~init:group_pos_map ~f:(fun i map r ->
                Map.set map ~key:r.number ~data:(i + 1))
          in
          map)
    in
    let finished =
      List.map t.finished ~f:(fun r ->
          let position = Map.find_exn group_pos_map r.number in
          Runner_result.update_gender_or_group_position r
            ~field:Runner_stats.Fields.position_group position)
    in

    { t with finished }

  let update_gender_and_group_positions t
      (simple_results : Simple_result.CourseResult.t list) =
    let runner_map : String.t Int.Map.t = Int.Map.empty in
    let runner_map =
      List.fold simple_results ~init:runner_map ~f:(fun map runner ->
          Map.set map ~key:runner.number ~data:runner.group.group)
    in

    (* update gender position for men *)
    let t = update_gender t runner_map ~gender_prefix:"V-" in
    (* update gender position for women *)
    let t = update_gender t runner_map ~gender_prefix:"M-" in

    (* update group position for each athlete. Groups are V-21A, M-40 etc. *)
    let t = update_group t runner_map in
    t
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : Response.ResultsTableResp.t) =
    let course_results =
      List.map resp.courses ~f:(CourseResult.of_resp resp.runners)
    in
    { course_results }
end

let parse_course_results_table data =
  let json = Yojson.Safe.from_string data in
  let results_table_resp = Response.ResultsTableResp.t_of_yojson json in
  Response.ResultsTableResp.filter_runners results_table_resp

let%expect_test "parse_course_results_table" =
  let filename = "/home/angel/Documents/ocaml/vkd/splits_resp.json" in
  let splits_resp = In_channel.read_all filename in
  let results_table_resp = parse_course_results_table splits_resp in
  let results_table = ResultsTable.of_resp results_table_resp in
  (* let out = Yojson.Safe.to_string (ResultsTable.yojson_of_t results_table) in *)
  (* let out = *)
  (*   List.nth_exn results_table.course_results 0 *)
  (*   |> CourseResult.yojson_of_t |> Yojson.Safe.to_string *)
  (* in *)
  let _ = results_table in
  (* Out_channel.write_all *)
  (*   "/home/angel/Documents/ocaml/vkd/course_results_test.json" ~data:out; *)
  (* Yojson.Safe.to_file *)
  (*   "/home/angel/Documents/ocaml/vkd/splits_resp_processed.json" *)
  (*   (ResultsTable.yojson_of_t results_table); *)
  printf "%s" "hello";
  (* printf "%s" out; *)
  [%expect {|
      hello |}]
