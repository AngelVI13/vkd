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
  rating_description : string;
  rating_change_description : string;
  last_event_description : string;
  name : string;
  rating : string;
  change : string;
  last_event : string;
  course : string;
  group : string;
  all : string;
  men : string;
  women : string;
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
    rating_description =
      "Performance rating is calculated based on a runner's position with \
       respect to other participants for each event using a multi-player \
       version of the Glicko2 algorithm. Each runner has a separate rating \
       value for each course (1, 2, 3 or D). The rating is calculated only for \
       events from „Vilniaus Ketvirtadieniai“ league. The Glicko rating \
       systems are used in chess, table tennis, online gaming and others to \
       determine the 'skill of a player.";
    rating_change_description =
      "This is the change of a runner's rating based on his performance in the \
       last event he/she participated.";
    last_event_description =
      "The date of the last event the runner participated in.";
    name = "Name";
    rating = "Rating";
    change = "Change";
    last_event = "Last Event";
    course = "Course";
    group = "Group";
    all = "All";
    men = "Men";
    women = "Women";
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
    rating_description =
      "Našumo reitingas apskaičiuojamas pagal bėgiko poziciją kitų dalyvių \
       atžvilgiu kiekvienoje rungtyje, naudojant daugelio žaidėjų „Glicko2“ \
       algoritmo versiją. Kiekvienas bėgikas kiekvienoje trasoje turi atskirą \
       įvertinimo vertę (1, 2, 3 arba D). Reitingas skaičiuojamas tik \
       „Vilniaus ketvirtadienių“ lygos renginiams. „Glicko“ įvertinimo \
       sistemos naudojamos šachmatuose, stalo tenise, internetiniuose \
       žaidimuose ir kituose žaidimuose, siekiant nustatyti žaidėjo įgūdžius.";
    rating_change_description =
      "Tai bėgiko reitingo pokytis, pagrįstas jo pasirodymu paskutinėje \
       rungtyje, kurioje jis/ji dalyvavo.";
    last_event_description = "Paskutinio bėgiko dalyvavimo renginyje data.";
    name = "Vardas";
    rating = "Reitingas";
    change = "Pokytis";
    last_event = "Paskutinis Ivykis";
    course = "Trasa";
    group = "Grupė";
    all = "Visi";
    men = "Vyrai";
    women = "Moterys";
  }

let translation_of_language (lang : language) : translations =
  match lang with English -> english | Lithuanian -> lithuanian
