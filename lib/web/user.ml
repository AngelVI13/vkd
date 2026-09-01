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
              ("autorange", `Bool true);
              ( "rangeselector",
                `Assoc
                  [
                    ( "buttons",
                      `List
                        [
                          `Assoc
                            [
                              ("count", `Int 6);
                              ("label", `String "6m");
                              ("step", `String "month");
                              ("stepmode", `String "backward");
                            ];
                          `Assoc
                            [
                              ("count", `Int 1);
                              ("label", `String "1y");
                              ("step", `String "year");
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
           ("traces", traces_json);
           ("layout", layout_json);
           ("config", config_json);
         ])
  in
  null
    [
      div [ id "rating-graph" ] [];
      script [ type_ "application/json"; id "plot-data" ] "%s" plot_data;
      script [ path_attr src Static.Assets.Js.Scripts.rating_graph_js ] "";
    ]

let runner_section (runner_info : Db.Types.RunnerInfo.t)
    (medals : Db.Types.Medals.t) =
  div
    [ class_ "box-contents" ]
    [
      span
        [ class_ "runner-info" ]
        [
          h1 [ class_ "runner-name" ] [ txt "%s" runner_info.name ];
          p [ class_ "runner-club" ] [ txt "%s" runner_info.club ];
        ];
      div
        [ class_ "runner-medals" ]
        (* TODO: all runner info including when they joined *)
        (* -------------- *)
        (* TODO: aggregated stats - second priority *)
        (* TODO: number of top1 top5 and top10 controls as % or totals ? *)
        (* TODO: tilt rate *)
        (* -------------- *)
        (* TODO: maybe show the following as graphs *)
        (* TODO: mistake stats *)
        (* TODO: performance stats *)
        (* \2604 ; \2776 *)
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
  match stats with
  | None -> null []
  | Some s -> (
      (* TODO: Augustas Velde is not disqualified and he has a rating but no
       position info ???? maybe because he is marked with group S ? *)
      (* TODO: in the case of disqualification just put `dsq` in a green block
       instead of the positions so it's not empty - test with Brazauskaitė Leokadija *)
      match
        Option.all [ s.overall_position; s.position_gender; s.position_group ]
      with
      | None -> null []
      | Some positions ->
          let overall_pos = List.nth_exn positions 0 in
          let gender_pos = List.nth_exn positions 1 in
          let group_pos = List.nth_exn positions 2 in
          div
            [ class_ "positions" ]
            [
              div
                [
                  class_ "overall"; title_ "%s" t.translations.overall_position;
                ]
                [ Icons.podium; strong [] [ txt "%d" overall_pos ] ];
              div
                [ class_ "gender"; title_ "%s" t.translations.gender_position ]
                [
                  (if String.equal Dbsportas.League.gender_men results.gender
                   then Icons.male
                   else Icons.female);
                  strong [] [ txt "%d" gender_pos ];
                ];
              div
                [ class_ "group"; title_ "%s" t.translations.group_position ]
                [ Icons.group; strong [] [ txt "%d" group_pos ] ];
            ])

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
           [ event_date; h2 [] [ txt "%s" t.translations.position ] ]
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

let history_section (t : Page_settings.t)
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

  let sections = List.mapi full_results ~f:(history_row t) in
  null
    [
      div
        [ class_ "angles number-menu number-menu--tabs menu-box-pop" ]
        [
          a
            [ class_ "nm-item to-activity active" ]
            [ txt "%s" t.translations.events ];
          a [ class_ "nm-item to-games" ] [ txt "%s" t.translations.stats ];
        ];
      div [ class_ "angle-content" ] [ div [ class_ "activity" ] sections ];
    ]

let profile (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list)
    (simple_results : Db.Types.SimpleResult.t list)
    (runner_info : Db.Types.RunnerInfo.t) (medals : Db.Types.Medals.t)
    (result_stats : Db.Types.ResultStats.t list) =
  (* TODO: add hovers with description *)
  (* TODO: add totals to page *)
  div
    [ class_ "profile-container page-small box" ]
    [
      runner_section runner_info medals;
      ratings_section t ratings simple_results;
      history_section t simple_results result_stats ratings;
    ]

let page (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list)
    (simple_results : Db.Types.SimpleResult.t list)
    (runner_info : Db.Types.RunnerInfo.t) (medals : Db.Types.Medals.t)
    (result_stats : Db.Types.ResultStats.t list) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [
          Header.elements t;
          div
            [ id "main-wrap" ]
            [ profile t ratings simple_results runner_info medals result_stats ];
        ];
    ]
