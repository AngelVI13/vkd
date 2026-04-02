open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Map = struct
  type t = { url : string } [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Maps = struct
  type t = { d : Map.t option; [@key "D"] [@yojson.option] default : Map.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Controls = struct
  type t = { finish_loc : float list [@key "F"] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module MapSettings = struct
  type t = { controls : Controls.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Settings = struct
  type t = {
    success : bool;
    key : string;
    title : string;
    map_settings : MapSettings.t;
    maps : Maps.t;
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end
