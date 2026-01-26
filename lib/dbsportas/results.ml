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
    time : int option;
    position : int option;
    overall_time : int option;
    overall_position : int option;
    absolute_time : int option;
  }
  [@@deriving yojson, fields]

  (* let from_time time overall_time absolute_time = *)
  (*   (* TODO: should these be options or keep it as -1 ?? *) *)
  (*   { time; overall_time; position = -1; overall_position = -1 } *)
end

module Splits = struct
  type t = Split.t list [@@deriving yojson]

  (* TODO: finish this *)
  let of_string s : t =
    let splits =
      String.split s ~on:'-'
      |> List.map ~f:(fun time ->
             let time = String.strip time in
             if String.(time = "") then 0 else Int.of_string time)
    in
    let _ = splits in
    []
end

type resultStatus = Finished | Dsq [@@deriving yojson]

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

  let of_resp (runner : RunnerResp.t) =
    let splits = Splits.of_string runner.splits in
    let finish = List.last_exn splits in
    (* TODO: is `value_exn` here safe ? *)
    let time = Option.value_exn finish.absolute_time - runner.start in

    Fields.create ~number:runner.number ~name:runner.name ~club:runner.club
      ~start:runner.start ~time
      ~status:(if runner.flag = 0 then Finished else Dsq)
      ~splits
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

  let of_resp (runners : RunnerResp.t list) (course : CourseResp.t) =
    let course_id = Int.of_string course.id in
    let runners = List.filter runners ~f:(fun r -> r.course_id = course_id) in
    let course_name =
      match List.hd runners with Some r -> r.course_name | None -> ""
    in
    let controls = String.split ~on:'-' course.controls in

    let runners = List.map runners ~f:RunnerResult.of_resp in
    let _ = runners in

    Fields.create ~course_name ~course_id ~controls ~finished:[] ~dsq:[]
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : ResultsTableResp.t) =
    let course_results =
      List.map resp.courses ~f:(CourseResult.of_resp resp.runners)
    in
    (* let runners_per_course = *)
    (*   List.sort_and_group resp.runners ~compare:(fun r1 r2 -> *)
    (*       String.compare r1.course_name r2.course_name) *)
    (*   |> List.map ~f:(fun runner_group -> *)
    (*          let first_runner = List.hd_exn runner_group in *)
    (*          let course = first_runner.course_name in *)
    (*          let runners = *)
    (*            List.map runner_group ~f:(fun r -> *)
    (*                let split_times = *)
    (*                  String.split ~on:'-' r.splits *)
    (*                  |> List.map ~f:(fun time -> *)
    (*                         let time = String.strip time in *)
    (*                         if String.(time = "") then 0 else Int.of_string time) *)
    (*                in *)
    (*                let time = List.last_exn split_times - r.start in *)
    (*                RunnerResult.Fields.create ~number:r.number ~name:r.name *)
    (*                  ~club:r.club ~start:r.start ~time *)
    (*                  ~status:(if r.flag = 0 then Good else Dsq) *)
    (*                  ~splits:[]) *)
    (*          in *)
    (*          let runners = *)
    (*            List.sort runners ~compare:(fun r1 r2 -> *)
    (*                Int.compare r1.time r2.time) *)
    (*          in *)
    (*          (* TODO: this currenlty sorts runners by finish time but dsq *)
    (*          runners should always be at the end of the list -> sort *)
    (*          based on status at the very end *) *)
    (*          (course, runners)) *)
    (* in *)
    (* let course_names = *)
    (*   List.map resp.runners ~f:(fun r -> *)
    (*       (r.course_name, Int.to_string r.course_id)) *)
    (*   |> List.dedup_and_sort ~compare:(fun (_, c1_name) (_, c2_name) -> *)
    (*          String.compare c1_name c2_name) *)
    (* in *)
    (* let course_results = *)
    (*   List.map resp.courses ~f:(fun course -> *)
    (*       let course_name, _ = *)
    (*         (* TODO: what happens if we don't have any runners per course? This will crash then ? *) *)
    (*         List.find_exn course_names ~f:(fun (_, id) -> *)
    (*             String.(id = course.id)) *)
    (*       in *)
    (*       let results = *)
    (*         List.filter runners_per_course ~f:(fun (group_course_name, _) -> *)
    (*             String.(group_course_name = course_name)) *)
    (*         |> List.map ~f:(fun (_, results) -> results) *)
    (*         |> List.hd_exn *)
    (*       in *)
    (*       CourseResult.Fields.create ~course_name ~course_id:course.id ~results *)
    (*         ~controls:(String.split ~on:'-' course.controls)) *)
    (* in *)
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
