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
  }
  [@@deriving show { with_path = false }, fields, sexp, yojson]

  let update_rd (t : t) (change_by : float) : t =
    let rd = t.rd +. change_by in
    let rd = if Float.(rd > 350.) then 350. else rd in
    { t with rd }
end
