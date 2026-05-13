open Core

type language = English | Lithuanian [@@deriving show { with_path = false }]

let language_of_abbrev = function
  | "en" -> English
  | "lt" -> Lithuanian
  | s -> failwith (sprintf "unsupported language abbreviation: %s\n" s)

let language_to_abbrev = function English -> "en" | Lithuanian -> "lt"

(* TODO: split this into its own folder (because these will grow like crazy)
   i18n -> translations.mli; english.ml; lithuanian.ml; language.ml *)

(* Define a module signature that each "implementor" must satisfy *)
module type TRANSLATIONS = sig
  val lang : string
  val ok : string
  val close : string
  val runner : string
end

module English : TRANSLATIONS = struct
  let lang = language_to_abbrev English
  let ok = "Ok"
  let close = "Close"
  let runner = "Runner"
end

module Lithuanian : TRANSLATIONS = struct
  let lang = language_to_abbrev Lithuanian
  let ok = "Ok"
  let close = "Uždaryti"

  (* TODO: how to handle different genders ??? *)
  let runner = "Bėgikas"
end

let translation_of_language (lang : language) : (module TRANSLATIONS) =
  match lang with
  | English -> (module English : TRANSLATIONS)
  | Lithuanian -> (module Lithuanian : TRANSLATIONS)
