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

(* TODO: implement calculatePlayersRatings & cleanPreviousMatches *)
