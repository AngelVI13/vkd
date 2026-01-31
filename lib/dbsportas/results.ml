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
  [@@deriving yojson, fields]

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
    splits : Split.t list;
  }
  [@@deriving fields, yojson]

  let of_resp (runner : RunnerResp.t) =
    let splits = Splits.of_string ~start:runner.start runner.splits in
    let finish = List.last_exn splits in
    (* `value_exn` here should be safe because everyone should have a finish time (i think?) *)
    let time = Option.value_exn finish.overall_time in

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

    (* TODO: update splits here (positions) *)
    let _ = runners in

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
