open Core

type playerMap = Player.t Int.Map.t

let show_playerMap t =
  let out =
    Map.fold t ~init:"" ~f:(fun ~key ~data acc ->
        sprintf "%s%d -> %s" acc key (Player.show data))
  in
  sprintf "{\n%s\n}\n" out

let pp_playerMap formatter t = Format.fprintf formatter "%s" (show_playerMap t)

type t = { settings : Settings.t; players_map : playerMap }
[@@deriving show { with_path = false }]

let create ~settings = { settings; players_map = Int.Map.empty }

let add_player t ~(id : int) ~(rating : float) ~(rd : float) ~(vol : float) =
  let player =
    Player.create ~rating ~rd ~vol ~tau:t.settings.tau
      ~default_rating:t.settings.initial_rating ~id
  in

  { t with players_map = Map.set t.players_map ~key:id ~data:player }

let add_player_with_defaults t ~(id : int) =
  add_player t ~id ~rating:t.settings.initial_rating ~rd:t.settings.rd
    ~vol:t.settings.vol

(** the outcome is from the point of view of player 1 *)
let add_match t ~(player1_id : int) ~(player2_id : int) ~(outcome : Outcome.t) =
  let players_map =
    Map.update t.players_map player1_id ~f:(fun player_opt ->
        let opponent = Map.find_exn t.players_map player2_id in
        match player_opt with
        | None -> assert false
        | Some player -> Player.add_result player ~opponent ~outcome)
  in
  let players_map =
    Map.update players_map player2_id ~f:(fun player_opt ->
        let opponent = Map.find_exn players_map player1_id in
        let outcome = Outcome.opposite outcome in
        match player_opt with
        | None -> assert false
        | Some player -> Player.add_result player ~opponent ~outcome)
  in
  { t with players_map }

let update_ratings t =
  {
    t with
    players_map =
      Map.map t.players_map ~f:(fun player -> Player.update_rank player);
  }

let players t = List.map (Map.data t.players_map) ~f:Player_rating.of_player

let%expect_test "update_ratings_1" =
  let settings =
    Settings.create ~tau:0.5 ~rd:200. ~vol:0.06 ~initial_rating:1500. ()
  in
  let glicko =
    create ~settings
    |> add_player_with_defaults ~id:1 (* Ryan *)
    |> add_player ~id:2 ~rating:1400. ~rd:30. ~vol:0.06 (* Bob *)
    |> add_player ~id:3 ~rating:1550. ~rd:100. ~vol:0.06 (* John *)
    |> add_player ~id:4 ~rating:1700. ~rd:300. ~vol:0.06 (* Mary *)
    (* Ryan beats Bob *)
    |> add_match ~player1_id:1 ~player2_id:2 ~outcome:Outcome.Victory
    (* Ryan loses to John *)
    |> add_match ~player1_id:1 ~player2_id:3 ~outcome:Outcome.Defeat
    (* Ryan loses to Mary *)
    |> add_match ~player1_id:1 ~player2_id:4 ~outcome:Outcome.Defeat
    |> update_ratings
  in
  let players = players glicko in
  List.iter players ~f:(fun p -> printf "%s\n" (Player_rating.show p));
  [%expect
    {|
    { id = 1; rating = 1464.05067054; rd = 151.516524124; vol = 0.0599959842865 }
    { id = 2; rating = 1398.14355823; rd = 31.6702152812; vol = 0.0599991237289 }
    { id = 3; rating = 1570.39474024; rd = 97.709168522; vol = 0.059999419472 }
    { id = 4; rating = 1784.42179013; rd = 251.565564532; vol = 0.0599990117637 } |}]

(* TODO: add support for full race and the results from it should be the same as this 
        [
          [Ryan], //Ryan won the race
          [Bob, John], //Bob and John 2nd position ex-aequo
          [Mary] // Mary 4th position
        ]
 *)
let%expect_test "update_ratings_1" =
  let settings =
    Settings.create ~tau:0.5 ~rd:200. ~vol:0.06 ~initial_rating:1500. ()
  in
  let glicko =
    create ~settings
    |> add_player_with_defaults ~id:1 (* Ryan *)
    |> add_player ~id:2 ~rating:1400. ~rd:30. ~vol:0.06 (* Bob *)
    |> add_player ~id:3 ~rating:1550. ~rd:100. ~vol:0.06 (* John *)
    |> add_player ~id:4 ~rating:1700. ~rd:300. ~vol:0.06 (* Mary *)
    (* Ryan beats Bob, John & Mary *)
    |> add_match ~player1_id:1 ~player2_id:2 ~outcome:Outcome.Victory
    |> add_match ~player1_id:1 ~player2_id:3 ~outcome:Outcome.Victory
    |> add_match ~player1_id:1 ~player2_id:4 ~outcome:Outcome.Victory
    (* NOTE: we don't add the same match "Ryan beat Bob" from Bob's perspective
       because we have done it above *)
    (* Bob loses to Ryan but draws John and beats Mary *)
    |> add_match ~player1_id:2 ~player2_id:3 ~outcome:Outcome.Draw
    (* John loses to Ryan but draws Bob and beats Mary *)
    |> add_match ~player1_id:3 ~player2_id:4 ~outcome:Outcome.Victory
    |> update_ratings
  in
  let players = players glicko in
  List.iter players ~f:(fun p -> printf "%s\n" (Player_rating.show p));
  [%expect
    {|
    { id = 1; rating = 1685.72145039; rd = 151.516557043; vol = 0.0600043923961 }
    { id = 2; rating = 1399.22100492; rd = 31.5691156995; vol = 0.0599954322606 }
    { id = 3; rating = 1539.88476704; rd = 93.0270600887; vol = 0.0599946096036 }
    { id = 4; rating = 1369.19439435; rd = 212.313018397; vol = 0.0600032317461 } |}]
