type t = { id : int; rating : float; rd : float; vol : float }
[@@deriving show { with_path = false }]

let of_player (player : Player.t) =
  {
    id = Player.id player;
    rating = Player.rating player;
    rd = Player.rd player;
    vol = Player.vol player;
  }
