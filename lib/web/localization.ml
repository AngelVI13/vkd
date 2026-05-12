open! Core

type translations = Ok | Close | Runner

(* Define a module signature that each "implementor" must satisfy *)
module type TRANSLATIONS = sig
  val ok : string
  val close : string
  val runner : string
end

(* Convert any TRANSLATIONS module to the variant type *)
let to_translation (module T : TRANSLATIONS) : translations -> string = function
  | Ok -> T.ok
  | Close -> T.close
  | Runner -> T.runner

module English : TRANSLATIONS = struct
  let ok = "Ok"
  let close = "Close"
  let runner = "Runner"
end

module Lithuanian : TRANSLATIONS = struct
  let ok = "Ok"
  let close = "Uždaryti"

  (* TODO: how to handle different genders ??? *)
  let runner = "Bėgikas"
end

type language = English | Lithuanian [@@deriving show { with_path = false }]

let language_of_abbrev = function
  | "en" -> English
  | "lt" -> Lithuanian
  | s -> failwith (sprintf "unsupported language abbreviation: %s\n" s)

let language_to_abbrev = function English -> "en" | Lithuanian -> "lt"

let translation_of_language = function
  | English -> to_translation (module English)
  | Lithuanian -> to_translation (module Lithuanian)
