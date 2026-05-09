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
  [@@deriving show { with_path = false }, fields, sexp]
end
