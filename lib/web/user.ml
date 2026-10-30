open Core
open Dream_html
open HTML

let head_elems (t : Page_settings.t) =
  Common.head_elems t.dark_mode
  @ [
      link
        [
          rel "stylesheet";
          type_ "text/css";
          path_attr href Static.Assets.Css.user_css;
        ];
      script [ src "https://cdn.plot.ly/plotly-3.7.0.min.js" ] "";
    ]

let rating_section (t : Page_settings.t) (course_id : string)
    (rating : Glicko2.Rating.Info.t option) (num_events : int) =
  let course_icon =
    match Common.ratingCourse_of_string course_id with
    | Course1 -> Icons.course1
    | Course2 -> Icons.course2
    | Course3 -> Icons.course3
    | CourseD -> Icons.bike_badge
  in
  let rating_value_class = "rating-value" in
  let event_info =
    if num_events > 0 then
      div
        [ class_ "course-event-num" ]
        [ txt "%d %s" num_events t.translations.events ]
    else null []
  in
  let rating_details =
    match rating with
    | None ->
        div
          [ class_ "rating-details" ]
          [
            div
              [
                class_ "%s rating-unknown" rating_value_class;
                title_ "%s" t.translations.rating_description;
              ]
              [ txt "?" ];
            div [ class_ "rating-extra" ] [ event_info ];
          ]
    | Some r ->
        let rating_change_extra =
          if Float.(r.rating_diff > 0.0) then "good"
          else if Float.(r.rating_diff < 0.0) then "bad"
          else "line"
        in
        (* TODO: if RD is still high then show the rating as uncertain *)
        div
          [ class_ "rating-details" ]
          [
            div
              [
                class_ "%s" rating_value_class;
                title_ "%s" t.translations.rating_description;
              ]
              [ txt "%.0f" r.rating ];
            div
              [ class_ "rating-extra" ]
              [
                (if Float.(r.rating_diff = 0.0) then null []
                 else
                   span
                     [
                       class_ "rating-change %s" rating_change_extra;
                       title_ "%s" t.translations.rating_change_description;
                     ]
                     [ txt "%.0f" r.rating_diff ]);
                event_info;
              ];
          ]
  in
  div
    [ class_ "course-rating" ]
    [
      div
        [ class_ "course-info" ]
        [
          course_icon;
          span []
            [
              h3
                [ class_ "course-name" ]
                [ txt "%s %s" course_id t.translations.course ];
              rating_details;
            ];
        ];
    ]

let rating_trace (t : Page_settings.t)
    ((course_id, ratings) : string * Glicko2.Rating.Info.t list) =
  if List.length ratings = 0 then None
  else
    let data =
      List.map ratings ~f:(fun rating ->
          [
            `String rating.event_date; `Int Float.(to_int (round rating.rating));
          ])
      |> List.transpose_exn
    in

    let dates = List.nth_exn data 0 in
    let ratings = List.nth_exn data 1 in

    let color =
      match Common.ratingCourse_of_string course_id with
      | Course1 -> "gold"
      | Course2 -> "silver"
      | Course3 -> "orange"
      | CourseD -> "indianred"
    in

    Some
      (`Assoc
         [
           ("type", `String "scatter");
           ("mode", `String "lines+markers");
           ("name", `String (sprintf "%s %s" t.translations.course course_id));
           ("x", `List dates);
           ("y", `List ratings);
           ("line", `Assoc [ ("color", `String color) ]);
           ("connectgaps", `Bool true);
         ])

let rating_graph (t : Page_settings.t)
    (ratings : (string * Glicko2.Rating.Info.t list) list) =
  (* NOTE: These colors are copied from the light & dark colors css variable --c-bg-page *)
  let bg_color =
    if String.(t.dark_mode = "1") then "hsl(37, 10%, 8%)"
    else "hsl(37, 10%, 92%)"
  in
  let traces_json =
    `List (List.map ratings ~f:(rating_trace t) |> List.filter_opt)
  in
  let layout_json =
    `Assoc
      [
        ("hovermode", `String "x unified");
        ("hoverdistance", `Int 100);
        ("dragmode", `Bool false);
        ("scrollZoom", `Bool false);
        ( "xaxis",
          `Assoc
            [
              ( "rangeselector",
                `Assoc
                  [
                    ( "buttons",
                      `List
                        [
                          `Assoc
                            [
                              ("count", `Int 3);
                              ("label", `String "3m");
                              ("step", `String "month");
                              ("stepmode", `String "backward");
                            ];
                          `Assoc
                            [
                              ("count", `Int 6);
                              ("label", `String "6m");
                              ("step", `String "month");
                              ("stepmode", `String "backward");
                            ];
                          `Assoc [ ("step", `String "all") ];
                        ] );
                    ("type", `String "date");
                  ] );
            ] );
        ( "yaxis",
          `Assoc [ ("autorange", `Bool true); ("type", `String "linear") ] );
        ("height", `Int 250);
        ( "margin",
          `Assoc
            [ ("l", `Int 60); ("r", `Int 60); ("t", `Int 40); ("b", `Int 40) ]
        );
        ("autosize", `Bool true);
        ("plot_bgcolor", `String bg_color);
        ("paper_bgcolor", `String bg_color);
      ]
  in
  let config_json =
    `Assoc
      [
        ("responsive", `Bool true);
        ("displayModeBar", `Bool false);
        (* ("frameMargins", `Float 0.5); *)
        (* ("fillFrame", `Bool true); *)
      ]
  in
  let plot_data =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("dark_mode", `String t.dark_mode);
           ("traces", traces_json);
           ("layout", layout_json);
           ("config", config_json);
         ])
  in
  null
    [
      div [ id "rating-graph" ] [];
      script [ type_ "application/json"; id "rating-plot-data" ] "%s" plot_data;
      script [ path_attr src Static.Assets.Js.Scripts.rating_graph_js ] "";
    ]

let runner_section (t : Page_settings.t) (runner_info : Db.Types.RunnerInfo.t)
    (medals : Db.Types.Medals.t) =
  div
    [ class_ "box-contents" ]
    [
      span
        [ class_ "runner-info" ]
        [
          h1 [ class_ "runner-name" ] [ txt "%s" runner_info.name ];
          div
            [ class_ "runner-club-join-date" ]
            [
              p [ class_ "runner-club" ] [ txt "%s" runner_info.club ];
              p
                [ class_ "runner-join-date" ]
                [
                  txt "(%s: %s)" t.translations.first_event
                    runner_info.join_date;
                ];
            ];
        ];
      div
        [ class_ "runner-medals" ]
        [
          div
            [ class_ "medal-num gold" ]
            [
              Icons.medal;
              div [] [ txt "%d" (Option.value ~default:0 medals.gold) ];
            ];
          div
            [ class_ "medal-num silver" ]
            [
              Icons.medal;
              div [] [ txt "%d" (Option.value ~default:0 medals.silver) ];
            ];
          div
            [ class_ "medal-num bronze" ]
            [
              Icons.medal;
              div [] [ txt "%d" (Option.value ~default:0 medals.bronze) ];
            ];
        ];
    ]

let ratings_section (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list)
    (simple_results : Db.Types.SimpleResult.t list) =
  let sorted_ratings_per_course =
    Common.all_of_ratingCourse
    |> List.map ~f:Common.show_ratingCourse
    |> List.map ~f:(fun course_id ->
           let latest_rating =
             List.filter ratings ~f:(fun rating ->
                 String.equal rating.course_id course_id)
             |> List.sort ~compare:(fun r1 r2 ->
                    String.compare r1.event_date r2.event_date)
           in
           (course_id, latest_rating))
  in
  (* TODO: maybe add a button to only show courses which we have rating for and
     have a 'Show All' button to reveal otherwise ? *)
  let rating_info =
    List.map sorted_ratings_per_course ~f:(fun (course_id, ratings) ->
        let num_events =
          List.filter simple_results ~f:(fun r ->
              String.equal r.course_id course_id)
          |> List.length
        in
        rating_section t course_id (List.last ratings) num_events)
  in

  null
    [
      div [ class_ "rating-info" ] rating_info;
      rating_graph t sorted_ratings_per_course;
    ]

let history_row_course_info (t : Page_settings.t)
    (results : Db.Types.SimpleResult.t) =
  div
    [ class_ "history-course" ]
    [ txt "%s %s" results.course_id t.translations.course ]

let history_row_rating_info (t : Page_settings.t)
    (rating : Glicko2.Rating.Info.t option) =
  match rating with
  | None -> null []
  | Some rating ->
      let rating_change_extra =
        if Float.(rating.rating_diff > 0.0) then "good"
        else if Float.(rating.rating_diff < 0.0) then "bad"
        else "line"
      in
      (* TODO: show ? if rating is still uncertain *)
      div
        [ class_ "history-rating" ]
        [
          txt "%.0f" rating.rating;
          (if Float.(rating.rating_diff = 0.0) then null []
           else
             span
               [
                 class_ "rating-change %s" rating_change_extra;
                 title_ "%s" t.translations.rating_change_description;
               ]
               [ txt "%.0f" rating.rating_diff ]);
        ]

let history_row_positions_info (t : Page_settings.t)
    (results : Db.Types.SimpleResult.t) (stats : Db.Types.ResultStats.t option)
    =
  let overall_pos, gender_pos, group_pos =
    match stats with
    | None -> ("?", "?", "?")
    | Some s ->
        let overall =
          Option.map s.overall_position ~f:Int.to_string
          |> Option.value ~default:"?"
        in
        let gender =
          Option.map s.gender_position ~f:Int.to_string
          |> Option.value ~default:"?"
        in
        let group =
          Option.map s.group_position ~f:Int.to_string
          |> Option.value ~default:"?"
        in
        (overall, gender, group)
  in

  let positions =
    if results.dsq = 1 then
      [ div [ class_ "dsq"; title_ "" ] [ strong [] [ txt "dsq" ] ] ]
    else
      [
        div
          [ class_ "overall"; title_ "%s" t.translations.overall_position ]
          [ Icons.podium; strong [] [ txt "%s" overall_pos ] ];
        div
          [ class_ "gender"; title_ "%s" t.translations.gender_position ]
          [
            (if String.equal Dbsportas.League.gender_men results.gender then
               Icons.male
             else Icons.female);
            strong [] [ txt "%s" gender_pos ];
          ];
        div
          [ class_ "group"; title_ "%s" t.translations.group_position ]
          [ Icons.group; strong [] [ txt "%s" group_pos ] ];
      ]
  in
  div [ class_ "positions" ] positions

let history_row (t : Page_settings.t) (i : int)
    ( (results : Db.Types.SimpleResult.t),
      (stats : Db.Types.ResultStats.t option),
      (rating : Glicko2.Rating.Info.t option) ) =
  let icon_class =
    if
      String.equal results.league_name
        Dbsportas.League.LeagueInfo.main_league_name
    then "icon-star"
    else "icon-rombus"
  in

  let event_date =
    h2 []
      [
        time
          [ datetime "%s" results.event_date ]
          [ txt "%s" results.event_date ];
      ]
  in

  section []
    [
      (if Int.(i = 0) then
         div
           [ class_ "history-header" ]
           [
             event_date;
             h2
               [ class_ "header-position" ]
               [ txt "%s" t.translations.position ];
           ]
       else event_date);
      div
        [ class_ "entries"; title_ "%s" results.league_name ]
        [
          div
            [ class_ "entry" ]
            [
              div [ class_ "icon %s" icon_class ] [];
              div
                [ class_ "event" ]
                (* TODO: make this into a clickable link *)
                [ txt "%s" results.location; history_row_course_info t results ];
              history_row_rating_info t rating;
              history_row_positions_info t results stats;
            ];
        ];
    ]

let history_tab (t : Page_settings.t)
    (simple_results : Db.Types.SimpleResult.t list)
    (result_stats : Db.Types.ResultStats.t list)
    (ratings : Glicko2.Rating.Info.t list) =
  (* TODO: this might be very slow if we have a lot of
     simple_results/ratings/stats - optimize as map if necessary *)
  let full_results =
    List.map simple_results ~f:(fun r ->
        let stats =
          List.find result_stats ~f:(fun s ->
              String.equal r.event_date s.event_date)
        in
        let rating =
          List.find ratings ~f:(fun rating ->
              String.equal r.event_date rating.event_date)
        in
        (r, stats, rating))
  in

  (* NOTE: we only show the latest 10 results in the history tab  *)
  let results_to_show = List.take full_results 10 in

  let sections = List.mapi results_to_show ~f:(history_row t) in
  [ div [ class_ "activity" ] sections ]

module UserStats = struct
  type t = {
    all_splits : int;
    top_1_splits : int;
    top_5_splits : int;
    top_10_splits : int;
  }
  [@@deriving fields]

  let empty () =
    { all_splits = 0; top_1_splits = 0; top_5_splits = 0; top_10_splits = 0 }

  let fold_stats (t : t) (stats : Db.Types.ResultStats.t) =
    {
      t with
      (* TODO: do we need all_splits in this record *)
      top_1_splits = t.top_1_splits + stats.best_splits;
      top_5_splits = t.top_5_splits + stats.top5_splits;
      top_10_splits = t.top_10_splits + stats.top10_splits;
    }
end

let split_stat ~label (value : int) (total : int) =
  div
    [ class_ "split-stat" ]
    [
      div [ class_ "split-stat-label" ] [ txt "%s" label ];
      div
        [ class_ "split-stat-container" ]
        [
          div [ class_ "split-stat-value with-percent" ] [ txt "%d" value ];
          div
            [ class_ "split-stat-percent" ]
            [
              txt "%.2f"
                Float.(
                  round_decimal ~decimal_digits:3
                    (100. * (of_int value / of_int total)));
            ];
        ];
    ]

let control_totals ~label (value : int) =
  div
    [ class_ "split-stat" ]
    [
      div [ class_ "split-stat-label" ] [ txt "%s" label ];
      div
        [ class_ "split-stat-container" ]
        [ div [ class_ "split-stat-value" ] [ txt "%d" value ] ];
    ]

let split_stats (t : Page_settings.t) (overall_stats : UserStats.t)
    (total_splits : int) =
  div
    [ class_ "split-stats" ]
    [
      (* TODO: make these look fine on small screens *)
      split_stat ~label:t.translations.best_splits overall_stats.top_1_splits
        total_splits;
      (* TODO: whats the correct translation of these in lithuanian,
               current translation is bad *)
      split_stat ~label:t.translations.top_5_splits overall_stats.top_5_splits
        total_splits;
      split_stat ~label:t.translations.top_10_splits overall_stats.top_10_splits
        total_splits;
      control_totals ~label:t.translations.control_points total_splits;
    ]

module AccumulatedStats = struct
  type t = {
    event_date : string list;
    course_id : string list;
    mistake_time : int list;
    mistake_num : int list;
    small_mistake_time : int list;
    small_mistake_num : int list;
    small_mistake_time_ratio : int list;
    small_mistake_num_ratio : int list;
    big_mistake_time : int list;
    big_mistake_num : int list;
    big_mistake_time_ratio : int list;
    big_mistake_num_ratio : int list;
    blunder_mistake_time : int list;
    blunder_mistake_num : int list;
    blunder_mistake_time_ratio : int list;
    blunder_mistake_num_ratio : int list;
    consecutive_mistakes : int list;
    tilt_rate : int list;
    mistake_cluster : Dbsportas.Runner_stats.mistakeCluster option list;
    mistakes_impact : Dbsportas.Runner_stats.mistakesImpact option list;
    race_execution : Dbsportas.Runner_stats.raceExecution option list;
    best_splits : int list;
    top5_splits : int list;
    top10_splits : int list;
    performance : int list;
    overall_position : int option list;
    gender_position : int option list;
    group_position : int option list;
    potential_time : int option list;
    potential_position : int option list;
  }
  [@@deriving fields]

  let empty () =
    {
      event_date = [];
      course_id = [];
      mistake_time = [];
      mistake_num = [];
      small_mistake_time = [];
      small_mistake_num = [];
      small_mistake_time_ratio = [];
      small_mistake_num_ratio = [];
      big_mistake_time = [];
      big_mistake_num = [];
      big_mistake_time_ratio = [];
      big_mistake_num_ratio = [];
      blunder_mistake_time = [];
      blunder_mistake_num = [];
      blunder_mistake_time_ratio = [];
      blunder_mistake_num_ratio = [];
      consecutive_mistakes = [];
      tilt_rate = [];
      mistake_cluster = [];
      mistakes_impact = [];
      race_execution = [];
      best_splits = [];
      top5_splits = [];
      top10_splits = [];
      performance = [];
      overall_position = [];
      gender_position = [];
      group_position = [];
      potential_time = [];
      potential_position = [];
    }

  let add (t : t) (result : Db.Types.ResultStats.t) =
    (* NOTE: here we prepend values because the values were sorted from newest
       to oldest and by prepending we make them in the oldest to newest order
       *)
    {
      event_date = result.event_date :: t.event_date;
      course_id = result.course_id :: t.course_id;
      mistake_time = result.mistake_time :: t.mistake_time;
      mistake_num = result.mistake_num :: t.mistake_num;
      small_mistake_time = result.small_mistake_time :: t.small_mistake_time;
      small_mistake_num = result.small_mistake_num :: t.small_mistake_num;
      small_mistake_time_ratio =
        result.small_mistake_time_ratio :: t.small_mistake_time_ratio;
      small_mistake_num_ratio =
        result.small_mistake_num_ratio :: t.small_mistake_num_ratio;
      big_mistake_time = result.big_mistake_time :: t.big_mistake_time;
      big_mistake_num = result.big_mistake_num :: t.big_mistake_num;
      big_mistake_time_ratio =
        result.big_mistake_time_ratio :: t.big_mistake_time_ratio;
      big_mistake_num_ratio =
        result.big_mistake_num_ratio :: t.big_mistake_num_ratio;
      blunder_mistake_time =
        result.blunder_mistake_time :: t.blunder_mistake_time;
      blunder_mistake_num = result.blunder_mistake_num :: t.blunder_mistake_num;
      blunder_mistake_time_ratio =
        result.blunder_mistake_time_ratio :: t.blunder_mistake_time_ratio;
      blunder_mistake_num_ratio =
        result.blunder_mistake_num_ratio :: t.blunder_mistake_num_ratio;
      consecutive_mistakes =
        result.consecutive_mistakes :: t.consecutive_mistakes;
      tilt_rate = result.tilt_rate :: t.tilt_rate;
      mistake_cluster = result.mistake_cluster :: t.mistake_cluster;
      mistakes_impact = result.mistakes_impact :: t.mistakes_impact;
      race_execution = result.race_execution :: t.race_execution;
      best_splits = result.best_splits :: t.best_splits;
      top5_splits = result.top5_splits :: t.top5_splits;
      top10_splits = result.top10_splits :: t.top10_splits;
      performance = result.performance :: t.performance;
      overall_position = result.overall_position :: t.overall_position;
      gender_position = result.gender_position :: t.gender_position;
      group_position = result.group_position :: t.group_position;
      potential_time = result.potential_time :: t.potential_time;
      potential_position = result.potential_position :: t.potential_position;
    }

  let to_json_string_list (t : t) field =
    let values = Field.get field t in
    List.map values ~f:(fun value -> `String value)

  let to_json_int_list (t : t) field =
    let values = Field.get field t in
    List.map values ~f:(fun value -> `Int value)

  (** Serialize to json *)
  let to_json (t : t) (translations : Localization.translations) =
    `Assoc
      [
        ("dates", `List (to_json_string_list t Fields.event_date));
        ( "performance",
          `Assoc
            [
              ("data", `List (to_json_int_list t Fields.performance));
              ("title", `String translations.stat_performance_vs_superman);
              ( "description",
                `String translations.stat_performance_vs_superman_description );
            ] );
        ( "mistake_time",
          `Assoc
            [
              ("data", `List (to_json_int_list t Fields.mistake_time));
              ("title", `String translations.stat_mistake_time);
              ("description", `String translations.stat_mistake_time_description);
            ] );
        ( "mistake_num",
          `Assoc
            [
              ("data", `List (to_json_int_list t Fields.mistake_num));
              ("title", `String translations.stat_mistake_num);
              ("description", `String translations.stat_mistake_num_description);
            ] );
        ( "tilt_rate",
          `Assoc
            [
              ("data", `List (to_json_int_list t Fields.tilt_rate));
              ("title", `String translations.stat_tilt_rate);
              ("description", `String translations.stat_tilt_rate_description);
            ] );
      ]
end

let stats_plot_data (translations : Localization.translations)
    (result_stats : Db.Types.ResultStats.t list) =
  let accumulated =
    List.fold result_stats
      ~init:(AccumulatedStats.empty ())
      ~f:AccumulatedStats.add
  in

  AccumulatedStats.to_json accumulated translations

let stats_data (t : Page_settings.t)
    (result_stats : Db.Types.ResultStats.t list) =
  let stats_data = stats_plot_data t.translations result_stats in
  let plot_data =
    `Assoc [ ("stats_data", stats_data); ("dark_mode", `String t.dark_mode) ]
    |> Yojson.Safe.to_string
  in

  script [ type_ "application/json"; id "stats-plot-data" ] "%s" plot_data

let stats_tab (t : Page_settings.t) (result_stats : Db.Types.ResultStats.t list)
    (total_splits : int) =
  let overall_stats =
    List.fold ~init:(UserStats.empty ()) ~f:UserStats.fold_stats result_stats
  in
  [
    div [ class_ "user-stats" ] [ split_stats t overall_stats total_splits ];
    hr [];
    div
      [ class_ "stats-graphs"; id "stats-graphs" ]
      [ div [ class_ "stats-graph"; id "stats-graph" ] [] ];
    stats_data t result_stats;
    script [ path_attr src Static.Assets.Js.Scripts.stats_graph_js ] "";
  ]

type userTab = History | Stats
[@@deriving show { with_path = false }, sexp, enumerate, eq]

let userTab_of_string s = userTab_of_sexp (Sexp.of_string s)

let tab_sections (t : Page_settings.t) ~(selected_tab : userTab)
    ~(runner_id : int) tab_content =
  let tabs =
    List.map all_of_userTab ~f:(fun tab ->
        let label =
          match tab with
          | History -> txt "%s" t.translations.events
          | Stats -> txt "%s" t.translations.stats
        in
        let property_list =
          if equal_userTab tab selected_tab then
            [ class_ "nm-item active load-indicator" ]
          else
            [
              class_ "nm-item load-indicator";
              path_attr Hx.get Paths.user_tab_w_scope t.translations.lang
                runner_id (show_userTab tab);
              Hx.trigger "click";
              Hx.target "#tab-section";
              Hx.swap "outerHTML";
              Hx.indicator ".load-indicator";
            ]
        in
        a property_list [ label ])
  in

  div
    [ id "tab-section" ]
    [
      div [ class_ "angles number-menu number-menu--tabs menu-box-pop" ] tabs;
      div [ class_ "angle-content" ] tab_content;
    ]

let profile (t : Page_settings.t) ~(tabs : node)
    (ratings : Glicko2.Rating.Info.t list)
    (simple_results : Db.Types.SimpleResult.t list)
    (runner_info : Db.Types.RunnerInfo.t) (medals : Db.Types.Medals.t) =
  div
    [ class_ "profile-container page-small box" ]
    [
      runner_section t runner_info medals;
      ratings_section t ratings simple_results;
      tabs;
    ]

let page (t : Page_settings.t) ~(tabs : node)
    (ratings : Glicko2.Rating.Info.t list)
    (simple_results : Db.Types.SimpleResult.t list)
    (runner_info : Db.Types.RunnerInfo.t) (medals : Db.Types.Medals.t) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [
          Header.elements t;
          div
            [ id "main-wrap" ]
            [ profile t ~tabs ratings simple_results runner_info medals ];
        ];
    ]
