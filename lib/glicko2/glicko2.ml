type t = { settings : Settings.t; players : Player.t list; player_idx : int }
[@@deriving show { with_path = false }]
