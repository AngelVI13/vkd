open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Core

(** This is the model that is stored & read from db *)
module Info = struct
  type t = {
    league_id : int;
    event_nr : int;
    event_date : string;
    course_id : string;
    runner_id : int;
    runner_name : string;
    runner_club : string;
    runner_gender : string;
    rating : float;
    rating_diff : float;
    rd : float;
    vol : float;
    position : int option;
  }
  [@@deriving show { with_path = false }, fields, sexp, yojson]

  let update_rd (t : t) (change_by : float) : t =
    let rd = t.rd +. change_by in
    let rd = if Float.(rd > 350.) then 350. else rd in
    { t with rd }

  let make ~league_id ~event_nr ~event_date ~course_id ~runner_id ~rating
      ~rating_diff ~rd ~vol ~runner_name ~runner_club ~runner_gender : t =
    {
      league_id = Int64.to_int_exn league_id;
      event_nr = Int64.to_int_exn event_nr;
      event_date;
      course_id;
      runner_id = Int64.to_int_exn runner_id;
      rating;
      rating_diff;
      rd;
      vol;
      runner_name;
      runner_club;
      runner_gender;
      (* NOTE: position is set only for display purposes and is different for
         different filters *)
      position = None;
    }
end
