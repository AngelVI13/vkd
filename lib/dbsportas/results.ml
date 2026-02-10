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
  [@@deriving fields, yojson]

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
    in
    let dsq =
      List.filter runners ~f:(fun r ->
          Runner_result.equal_resultStatus r.status Runner_result.Dsq)
    in

    (* TODO: calculate mistakes here and add them to the split record *)
    Fields.create ~course_name ~course_id ~controls ~finished ~dsq
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : Response.ResultsTableResp.t) =
    let course_results =
      List.map resp.courses ~f:(CourseResult.of_resp resp.runners)
    in
    { course_results }
end

let parse_course_results_table json_chan =
  let json = Yojson.Safe.from_channel json_chan in
  Response.ResultsTableResp.t_of_yojson json

let%expect_test "parse_course_results_table" =
  let filename = "/home/angel/Documents/ocaml/vkd/splits_resp.json" in
  let splits_resp = In_channel.create filename in
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
  [%expect
    {| {"course_results":[{"course_name":"1,2","course_id":"426148","controls":["49","31","32","42","41","45","47","35","40","36","44","33","37","46","30","43","39","48","38","34","69","FIN"],"results":[]},{"course_name":"3","course_id":"426149","controls":["31","44","42","36","45","40","41","33","37","49","39","48","46","38","34","69","FIN"],"results":[]},{"course_name":"4","course_id":"426150","controls":["37","32","44","33","49","46","38","30","43","48","34","69","FIN"],"results":[]},{"course_name":"D","course_id":"426152","controls":["51","52","60","54","55","59","57","58","37","34","56","46","48","69","FIN"],"results":[]},{"course_name":"P","course_id":"426151","controls":["37","31","49","48","46","56","34","69","FIN"],"results":[]}]} |}]
