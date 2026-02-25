open Core

type t = {
  tau : float;
  rating : float;
  rd : float;
  vol : float;
  default_rating : float;
  id : int;
  adv_ranks : float list;
  adv_rds : float list;
  outcomes : Outcome.t list;
}
[@@deriving show { with_path = false }]

let rating t = (t.rating *. Constants.scaling_factor) +. t.default_rating

let set_rating t ~rating =
  { t with rating = (rating -. t.default_rating) /. Constants.scaling_factor }

let rd t = t.rd *. Constants.scaling_factor
let set_rd t ~rd = { t with rd = rd /. Constants.scaling_factor }
let vol t = t.vol
let set_vol t ~vol = { t with vol }

let create ~rating ~rd ~vol ~tau ~default_rating ~id =
  {
    tau;
    default_rating;
    rating = 0.0;
    rd = 0.0;
    vol = 0.0;
    id;
    adv_ranks = [];
    adv_rds = [];
    outcomes = [];
  }
  |> set_rating ~rating |> set_rd ~rd |> set_vol ~vol

let add_result t ~opponent ~outcome =
  {
    t with
    (* NOTE: here we are using the raw rating & rd values from the record *)
    adv_ranks = [ opponent.rating ] @ t.adv_ranks;
    adv_rds = [ opponent.rd ] @ t.adv_rds;
    outcomes = [ outcome ] @ t.outcomes;
  }

(* TODO: implement these *)
let update_rank t = t

let predict t ~opponent =
  let _ = (opponent, t) in
  Outcome.Victory

let%expect_test "add_result" =
  let p1 =
    create ~rating:1000.0 ~rd:350.0 ~vol:0.05 ~tau:0.6 ~default_rating:1500.0
      ~id:1
  in
  let p2 =
    create ~rating:1001.0 ~rd:350.0 ~vol:0.05 ~tau:0.6 ~default_rating:1500.0
      ~id:2
  in

  printf "%f (%f)| %f (%f)\n" p1.rating (rating p1) p2.rating (rating p2);

  let new_p1 = add_result p1 ~opponent:p2 ~outcome:Outcome.Defeat in
  printf "%s\n" (show new_p1);
  [%expect {|
    -2.878231 (1000.000000)| -2.872475 (1001.000000)
    { tau = 0.6; rating = -2.87823124631; rd = 2.01476187242; vol = 0.05;
      default_rating = 1500.; id = 1; adv_ranks = [-2.87247478382];
      adv_rds = [2.01476187242]; outcomes = [Defeat] } |}]
