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
  [@@deriving yojson_of]

  let t_of_yojson (json : Yojson.Safe.t) : t =
    let open Yojson.Safe.Util in
    let number = json |> member "number" |> to_int in
    (* NOTE: in some special cases name and/or club can be set to null. Here we
       just make them into empty strings *)
    (* TODO: Is it a problem that name and/or club can be an empty string ??? *)
    let name =
      match json |> member "name" with
      | `Null -> ""
      | `String s -> s
      | _ ->
          failwith
            (sprintf "failed to parse name for %s" (Yojson.Safe.to_string json))
    in
    let club =
      match json |> member "club" with
      | `Null -> ""
      | `String s -> s
      | _ ->
          failwith
            (sprintf "failed to parse club for %s" (Yojson.Safe.to_string json))
    in

    let course_name = json |> member "class" |> to_string in
    let course_id = json |> member "course_id" |> to_int in
    let start = json |> member "start" |> to_int in
    let flag = json |> member "flag" |> to_int in
    let splits = json |> member "splits" |> to_string in
    { number; name; club; course_name; course_id; start; flag; splits }
end

module ResultsTableResp = struct
  type t = {
    err : bool;
    courses : CourseResp.t list;
    runners : RunnerResp.t list;
  }
  [@@deriving yojson]

  (* filter runners who don't have a name *)
  let filter_runners t =
    let runners =
      List.filter t.runners ~f:(fun r -> not (String.is_empty r.name))
    in
    { t with runners }
end
