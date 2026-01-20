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
    control : string;
    time : int;
    position : int;
    current_time : int;
    current_position : int;
  }
  [@@deriving yojson, fields]
end

type resultStatus = Good | Dsq [@@deriving yojson]

module RunnerResult = struct
  type t = {
    number : int;
    name : string;
    club : string;
    start : int;
    status : resultStatus;
    time : int;
    splits : Split.t list;
  }
  [@@deriving fields, yojson]
end

module CourseResult = struct
  type t = {
    course_name : string;
    course_id : string;
    controls : string list;
    results : RunnerResult.t list;
  }
  [@@deriving fields, yojson]
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : ResultsTableResp.t) =
    let runners_per_course =
      List.sort_and_group resp.runners ~compare:(fun r1 r2 ->
          String.compare r1.course_name r2.course_name)
      |> List.map ~f:(fun runner_group ->
             let first_runner = List.hd_exn runner_group in
             let course = first_runner.course_name in
             let runners =
               List.map runner_group ~f:(fun r ->
                   let split_times =
                     String.split ~on:'-' r.splits
                     |> List.map ~f:(fun time ->
                            let time = String.strip time in
                            if String.(time = "") then 0 else Int.of_string time)
                   in
                   let time = List.last_exn split_times - r.start in
                   RunnerResult.Fields.create ~number:r.number ~name:r.name
                     ~club:r.club ~start:r.start ~time
                     ~status:(if r.flag = 0 then Good else Dsq)
                     ~splits:[])
             in

             let runners =
               List.sort runners ~compare:(fun r1 r2 ->
                   Int.compare r1.time r2.time)
             in
             (* TODO: this currenlty sorts runners by finish time but dsq
             runners should always be at the end of the list -> sort
             based on status at the very end *)
             (course, runners))
    in

    let course_names =
      List.map resp.runners ~f:(fun r ->
          (r.course_name, Int.to_string r.course_id))
      |> List.dedup_and_sort ~compare:(fun (_, c1_name) (_, c2_name) ->
             String.compare c1_name c2_name)
    in
    let course_results =
      List.map resp.courses ~f:(fun course ->
          let course_name, _ =
            (* TODO: what happens if we don't have any runners per course? This will crash then ? *)
            List.find_exn course_names ~f:(fun (_, id) ->
                String.(id = course.id))
          in
          let results =
            List.filter runners_per_course ~f:(fun (group_course_name, _) ->
                String.(group_course_name = course_name))
            |> List.map ~f:(fun (_, results) -> results)
            |> List.hd_exn
          in

          CourseResult.Fields.create ~course_name ~course_id:course.id ~results
            ~controls:(String.split ~on:'-' course.controls))
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
  printf "%s" (Yojson.Safe.to_string (ResultsTable.yojson_of_t results_table));
  [%expect
    {| {"course_results":[{"course_name":"1,2","course_id":"426148","controls":["49","31","32","42","41","45","47","35","40","36","44","33","37","46","30","43","39","48","38","34","69","FIN"],"results":[]},{"course_name":"3","course_id":"426149","controls":["31","44","42","36","45","40","41","33","37","49","39","48","46","38","34","69","FIN"],"results":[]},{"course_name":"4","course_id":"426150","controls":["37","32","44","33","49","46","38","30","43","48","34","69","FIN"],"results":[]},{"course_name":"D","course_id":"426152","controls":["51","52","60","54","55","59","57","58","37","34","56","46","48","69","FIN"],"results":[]},{"course_name":"P","course_id":"426151","controls":["37","31","49","48","46","56","34","69","FIN"],"results":[]}]} |}]
