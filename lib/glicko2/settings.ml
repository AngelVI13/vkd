type t = {
  tau : float;
      (** The system constant which constrains changes in volatility (tau). *)
  initial_rating : float;  (** The initial rating for players. *)
  rd : float;  (** The initial rating deviation for players. *)
  vol : float;  (** The initial volatility for players. *)
}
[@@deriving show { with_path = false }]

let create ?(tau = 0.5) ?(initial_rating = 1500.0) ?(rd = 350.0) ?(vol = 0.06)
    () =
  { tau; initial_rating; rd; vol }
