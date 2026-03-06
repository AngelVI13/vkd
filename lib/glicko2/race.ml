open Core

module Stats = struct
  type t = { rating : float; rd : float; vol : float }
  [@@deriving show { with_path = false }, fields]
end

module Participant = struct
  type t = { id : int; stats : Stats.t option }
  [@@deriving show { with_path = false }, fields]
end

module Race = struct
  type t = Participant.t list list [@@deriving show { with_path = false }]
end
