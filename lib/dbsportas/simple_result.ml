open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module AgeGroup = struct
  type t = {
    url : string; (* url to overall league standings for the age group *)
    group : string;
  }
  [@@deriving show { with_path = false }, fields, yojson]
end

module CourseResult = struct
  type t = {
    position : int option;
    number : int;
        (* no clue what this number means, maybe its participant id or sth *)
    group : AgeGroup.t;
    name : string; (* stored in the format: LASTNAME FIRSTNAME *)
    club : string;
    time : string;
    points : int;
    pace : string option; (* in min/km i.e.: 6:40 OR `dsq`*)
  }
  [@@deriving fields, yojson]

  let pp ppf r =
    let position =
      match r.position with None -> "" | Some pos -> sprintf "%d" pos
    in
    let pace = match r.pace with None -> "" | Some pace -> pace in
    Format.fprintf ppf
      "{ position: %s; number: %d; group: %s; name: %s; club: %s; time: %s; \
       points: %d; pace: %s }"
      position r.number (AgeGroup.show r.group) r.name r.club r.time r.points
      pace

  let show r = Format.asprintf "%a" pp r
end
