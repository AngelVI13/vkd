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
  type_to_search_runner : string;
  performance_ratings : string;
  powered_by : string;
  stats : string;
  position : string;
  overall_position : string;
  gender_position : string;
  group_position : string;
  first_event : string;
  best_splits : string;
  top_5_splits : string;
  top_10_splits : string;
  control_points : string;
  stats_analysis : string;
  stat_performance_vs_superman : string;
  stat_mistake_time : string;
  stat_mistake_num : string;
  stat_tilt_rate : string;
  stat_performance_vs_superman_description : string;
  stat_mistake_time_description : string;
  stat_mistake_num_description : string;
  stat_tilt_rate_description : string;
  about_stats : string;
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
    type_to_search_runner = "Type to search for a runner";
    performance_ratings = "Performance Ratings";
    powered_by = "Powered by";
    stats = "Stats";
    position = "Position";
    overall_position = "Position overall";
    gender_position = "Position by gender";
    group_position = "Position by group (V-21, M-45, etc)";
    first_event = "First event";
    best_splits = "Best splits";
    top_5_splits = "Top 5 splits";
    top_10_splits = "Top 10 splits";
    control_points = "Control points";
    stats_analysis = "Stats Analysis";
    stat_performance_vs_superman = "Performance vs Superman (%)";
    stat_mistake_time = "Total mistake time (minutes)";
    stat_mistake_num = "Number of mistakes";
    stat_tilt_rate = "Error-Caused Instability (%)";
    stat_performance_vs_superman_description =
      "Your performance compared to the best split times for each control \
       excluding any significant mistakes. For example, if your \"Performance \
       vs Superman\" is 50% this means that if the best split for a control is \
       1:00 then you ran that split for ~1:30";
    stat_mistake_time_description =
      "The sum of all your mistakes throughout the course. The mistake time \
       for each control is calculated based on your \"Performance vs \
       Superman\" percentage. First we determine what your control split time \
       should be by taking the best time for that split and normalizing it by \
       your performance percentage. The mistake time is the difference between \
       that estimated time and your actual split time. For example, if your \
       split time to control 1 is 0:57 and the best split is 0:30. If your \
       performance percentage is 50% then your estimated split time for that \
       control is 0:45 and mistake time will be 0:12 (0:57-0:45). Differences \
       of less than 10 seconds are not counted as mistakes";
    stat_mistake_num_description =
      "The total number of mistakes for the whole course. Any mistake of 10 \
       seconds or more is counted.";
    stat_tilt_rate_description =
      "The tendency to make a mistake right after making a mistake. For \
       example, if you make a mistake at control 3 and then also make a \
       mistake at control 4, but managed to find control 5 without mistakes - \
       your \"Error-Caused Instability\" would be 50%. In other words, this \
       stat tells you that one mistake makes you 50% more likely to make \
       another mistake straight after. If, however, after making a mistake at \
       control 3 then you recover and find control 4 without making a mistake \
       then your \"Error-Caused Instability\" would be 0%. Another example is \
       if you make 3 mistakes in a row and find the rest of the controls \
       without a mistake, then your \"Error-Caused Instability\" will be 75%.";
    about_stats = "About these statistics";
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
    type_to_search_runner = "Įveskite tekstą, jei norite ieškoti bėgiko";
    performance_ratings = "Našumo reitingai";
    powered_by = "Sukurta naudojant";
    stats = "Statistika";
    position = "Pozicija";
    overall_position = "Bendroji pozicija";
    gender_position = "Pozicija pagal lytį";
    group_position = "Pozicija pagal grupę (V-21, M-45, etc)";
    first_event = "Pirmasis etapas";
    best_splits = "greičiausi tarpiniai laikai";
    top_5_splits = "5 greičiausi tarpiniai laikai";
    top_10_splits = "10 greičiausi tarpiniai laikai";
    control_points = "kontrolės punktai";
    stats_analysis = "statistinė analizė";
    stat_performance_vs_superman = "Rezultatas prieš Antžmogis (%)";
    stat_mistake_time = "Visiško klydimo metas (minutės)";
    stat_mistake_num = "Klaidų skaičius";
    stat_tilt_rate = "Klaidų sukeltas nestabilumas (%)";
    stat_performance_vs_superman_description =
      "Jūsų pasirodymas, palygintas su geriausiais tarpiniais laikais \
       kiekviename kontroliniame punkte (neskaičiuojant didelių klaidų). \
       Pavyzdžiui, jei jūsų rodiklis \"Rezultatas prieš Antžmogis\" yra 50%, \
       tai reiškia, kad jei geriausias laikas pasiekti 7-ąjį kontrolinį punktą \
       buvo 1:00, jūs šią atkarpą įveikėte per ~1:30.";
    stat_mistake_time_description =
      "Visų jūsų per trasoje padarytų klaidų suma. Klaidoms sugaištas laikas \
       kiekviename kontroliniame punkte apskaičiuojamas remiantis jūsų \
       \"Rezultatas prieš Antžmogis\" rodikliu. Pirmiausia nustatome, koks \
       turėtų būti jūsų laikas iki konkretaus kontrolinio punkto: paimame \
       geriausią laiką tame etape ir pakoreguojame jį pagal jūsų rezultatyvumo \
       procentą. Klaidoms sugaištas laikas – tai skirtumas tarp šio \
       apskaičiuoto laiko ir jūsų realiai užtrukto laiko. Pavyzdžiui, jei jūsų \
       laikas iki 1-ojo kontrolinio punkto yra 0:57, o geriausias laikas tame \
       etape – 0:30, o jūsų rezultatyvumo rodiklis yra 50 %, tai apskaičiuotas \
       laikas iki šio punkto būtų 0:45, o klaidoms sugaištas laikas – 0:12 \
       (0:57 – 0:45). Mažesni nei 10 sekundžių skirtumai klaidomis nelaikomi. ";
    stat_mistake_num_description =
      "Bendras klaidų skaičius visoje trasoje. Įskaitoma bet kuri klaida, \
       trunkanti 10 sekundžių ar ilgiau.";
    stat_tilt_rate_description =
      "Tai polinkis padaryti klaidą iškart po to, kai jau buvo padaryta \
       klaida. Pavyzdžiui, jei padarote klaidą prie 3-iojo kontrolinio punkto, \
       o paskui – ir prie 4-ojo, tačiau 5-ąjį punktą randate be klaidų, jūsų \
       \"Klaidų sukeltas nestabilumas\" sieks 50%. Kitaip tariant, šis \
       rodiklis rodo, kad padarius vieną klaidą, tikimybė iškart po jos \
       padaryti dar vieną klaidą padidėja 50%. Tačiau jei, padarę klaidą prie \
       3-iojo punkto, susikaupiate ir 4-ąjį punktą randate be klaidų, jūsų \
       \"Klaidų sukeltas nestabilumas\" bus 0%. Kitas pavyzdys: jei padarote 3 \
       klaidas iš eilės, o likusius punktus randate be klaidų, jūsų \"Klaidų \
       sukeltas nestabilumas\" bus 75%.";
    about_stats = "Apie šią statistiką";
  }

let translation_of_language (lang : language) : translations =
  match lang with English -> english | Lithuanian -> lithuanian
