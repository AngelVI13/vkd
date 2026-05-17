open Core

type language = English | Lithuanian
[@@deriving show { with_path = false }, enumerate]

let language_of_abbrev = function
  | "en" -> English
  | "lt" -> Lithuanian
  | s -> failwith (sprintf "unsupported language abbreviation: %s\n" s)

let language_to_abbrev = function English -> "en" | Lithuanian -> "lt"

(* TODO: split this into its own folder (because these will grow like crazy)
   i18n -> translations.mli; english.ml; lithuanian.ml; language.ml *)

(* Define a module signature that each "implementor" must satisfy *)
type translations = {
  lang : string;
  ok : string;
  close : string;
  runner : string;
  events : string;
  leagues : string;
  runners : string;
}
[@@deriving show { with_path = false }]

let english =
  {
    lang = language_to_abbrev English;
    ok = "Ok";
    close = "Close";
    runner = "Runner";
    events = "Events";
    leagues = "Leagues";
    runners = "Runners";
  }

let lithuanian =
  {
    lang = language_to_abbrev Lithuanian;
    ok = "Ok";
    close = "Uždaryti";
    (* TODO: how to handle different genders ??? *)
    runner = "Bėgikas";
    events = "Etapai";
    leagues = "Lygos";
    runners = "Bėgikai";
  }

let translation_of_language (lang : language) : translations =
  match lang with English -> english | Lithuanian -> lithuanian
