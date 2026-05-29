type t = {
  translations : Localization.translations;
  dark_mode : string; (* "1" or "0" *)
}
[@@deriving show { with_path = false }, fields]
