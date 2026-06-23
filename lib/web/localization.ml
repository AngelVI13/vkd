open Core

type language = English | Lithuanian
[@@deriving show { with_path = false }, enumerate, eq]

let language_of_abbrev = function
  | "en" -> English
  | "lt" -> Lithuanian
  | s -> failwith (sprintf "unsupported language abbreviation: %s\n" s)

let language_to_abbrev = function English -> "en" | Lithuanian -> "lt"

let language_flag = function
  | English -> Static.Assets.Images.flag_gbr_svg
  | Lithuanian -> Static.Assets.Images.flag_ltu_svg

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
  prev_event : string;
  next_event : string;
  rating_uncertainty : string;
  name : string;
  rating : string;
  change : string;
  last_event : string;
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
    prev_event = "Past Event";
    next_event = "Next Event";
    rating_uncertainty =
      "Runner has not participated in enough „Vilniaus Ketvirtadieniai“ events \
       to establish a reliable rating or has missed too many events so his/her \
       rating is considered unreliable.";
    name = "Name";
    rating = "Rating";
    change = "Change";
    last_event = "Last Event";
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
    prev_event = "Praėjęs Etapas";
    next_event = "Sekantis Etapas";
    rating_uncertainty =
      "Bėgikas/ė nedalyvavo pakankamai „Vilniaus ketvirtadienio“ renginiuose, \
       kad būtų galima nustatyti patikimą įvertinimą, arba praleido per daug \
       renginių, todėl jo/jos įvertinimas laikomas nepatikimu.";
    name = "Vardas";
    rating = "Reitingas";
    change = "Pokytis";
    last_event = "Paskutinis Ivykis";
  }

let translation_of_language (lang : language) : translations =
  match lang with English -> english | Lithuanian -> lithuanian
