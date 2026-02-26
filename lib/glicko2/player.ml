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
  outcomes : float list;
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

let id t = t.id

let add_result t ~opponent ~outcome =
  {
    t with
    (* NOTE: here we are using the raw rating & rd values from the record *)
    adv_ranks = [ opponent.rating ] @ t.adv_ranks;
    adv_rds = [ opponent.rd ] @ t.adv_rds;
    outcomes = [ Outcome.int_of_t outcome ] @ t.outcomes;
  }

let has_played t = not (List.is_empty t.outcomes)

(** Calculates and updates the player's rating deviation for the beginning of a
    rating period. *)
let pre_rating_rd t =
  let open Float in
  { t with rd = sqrt ((t.rd ** 2.) + (t.vol ** 2.)) }

(** Glicko2 g(RD) function *)
let g_fun rd =
  let open Float in
  1. / sqrt (1. + (3. * (rd ** 2.) / (pi ** 2.)))

(* Glicko2 E function *)
let e_fun t p2_rating p2_rd =
  let open Float in
  1. / (1. + exp (-1. * g_fun p2_rd * (t.rating - p2_rating)))

(** The delta function of the Glicko2 system. Calculation of the estimated
    improvement in rating (step 4 of the algorithm) *)
let delta t v =
  let open Float in
  let temp_sum =
    List.foldi t.adv_ranks ~init:0. ~f:(fun i sum rank ->
        let adv_rd = List.nth_exn t.adv_rds i in
        let outcome = List.nth_exn t.outcomes i in
        let new_value = g_fun adv_rd * (outcome - e_fun t rank adv_rd) in
        sum + new_value)
  in
  v * temp_sum

(**Calculation of the estimated variance of the player's rating based on game
   outcomes *)
let variance t =
  let open Float in
  let temp_sum =
    List.foldi t.adv_ranks ~init:0. ~f:(fun i sum rank ->
        let adv_rd = List.nth_exn t.adv_rds i in
        let temp_e = e_fun t rank adv_rd in
        let new_value = (g_fun adv_rd ** 2.) * temp_e * (1. - temp_e) in
        sum + new_value)
  in
  1. / temp_sum

let make_f t delta v a =
  let open Float in
  let f x =
    exp x
    * ((delta ** 2.) - (t.rd ** 2.) - v - exp x)
    / (2. * (((t.rd ** 2.) + v + exp x) ** 2.))
    - ((x - a) / (t.tau ** 2.))
  in
  f

let volatility_algorithm t v delta =
  let open Float in
  (* Step 5.1 *)
  let a = log (t.vol ** 2.) in
  let f = make_f t delta v a in
  let epsilon = 0.0000001 in

  (* Step 5.2 *)
  let b =
    if delta ** 2. > (t.rd ** 2.) + v then log ((delta ** 2.) - (t.rd ** 2.) - v)
    else
      let k = ref 0. in
      while f (a - (!k * t.tau)) < 0. do
        k := !k + 1.
      done;
      a - (!k * t.tau)
  in

  (* Step 5.3 *)
  let fa = ref (f a) in
  let fb = ref (f b) in

  let a = ref a in
  let b = ref b in

  (* Step 5.4 *)
  while abs (!b - !a) > epsilon do
    let c = !a + ((!a - !b) * !fa / (!fb - !fa)) in
    let fc = f c in
    fa :=
      if fc * !fb <= 0. then (
        a := !b;
        !fb)
      else !fa / 2.;
    b := c;
    fb := fc
  done;

  (* Step 5.5 *)
  exp (!a / 2.)

(** Calculates the new rating and rating deviation of the player. Follows the
    steps of the algorithm described at http://www.glicko.net/glicko/glicko2.pdf
*)
let update_rank t =
  if not (has_played t) then
    (* Applies only the Step 6 of the algorithm *)
    pre_rating_rd t
  else
    (* Step 1 : done by Player.create *)
    (* Step 2 : done by set_rating and set_rd *)

    (* Step 3 *)
    let v = variance t in
    (* Step 4 *)
    let delta = delta t v in
    (* Step 5 *)
    let vol = volatility_algorithm t v delta in
    let t = { t with vol } in
    (* Step 6 *)
    let t = pre_rating_rd t in

    (* Step 7 *)
    let rd = 1. /. sqrt ((1. /. (t.rd ** 2.)) +. (1. /. v)) in
    let t = { t with rd } in

    let temp_sum =
      List.foldi t.adv_ranks ~init:0. ~f:(fun i sum rank ->
          let open Float in
          let adv_rd = List.nth_exn t.adv_rds i in
          let outcome = List.nth_exn t.outcomes i in
          let new_value = g_fun adv_rd * (outcome - e_fun t rank adv_rd) in
          sum + new_value)
    in

    let rating = t.rating +. ((t.rd ** 2.) *. temp_sum) in
    let t = { t with rating } in

    (* Step 8: done by calling Player.rating and Player.rd *)
    t

let predict t ~opponent =
  let open Float in
  let diff_rd = sqrt ((t.rd ** 2.) + (opponent.rd ** 2.)) in
  1. / (1. + (exp (-1.) * g_fun diff_rd * (t.rating - opponent.rating)))

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
  [%expect
    {|
    -2.878231 (1000.000000)| -2.872475 (1001.000000)
    { tau = 0.6; rating = -2.87823124631; rd = 2.01476187242; vol = 0.05;
      default_rating = 1500.; id = 1; adv_ranks = [-2.87247478382];
      adv_rds = [2.01476187242]; outcomes = [0.] } |}]
