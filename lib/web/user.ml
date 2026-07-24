open! Core
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
      (* script [ src "https://cdn.plot.ly/plotly-3.7.0.min.js" ] ""; *)
      script [ path_attr src Static.Assets.Js.Plotly.plotly_3_7_0_min_js ] "";
    ]

let rating_section (t : Page_settings.t) (course_id : string)
    (rating : Glicko2.Rating.Info.t option) (num_events : int) =
  (* TODO: add hovers with description *)
  let course_icon =
    match course_id with
    | "1" -> Icons.course1
    | "2" -> Icons.course2
    | "3" -> Icons.course3
    | "D" -> Icons.bike_badge
    | _ -> null []
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
                span
                  [
                    class_ "rating-change %s" rating_change_extra;
                    title_ "%s" t.translations.rating_change_description;
                  ]
                  [ txt "%.0f" r.rating_diff ];
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

let rating_graph (ratings : (string * Glicko2.Rating.Info.t list) list) =
  let _ = ratings in
  (* 
2015-02-17,127.489998,128.880005,126.919998,127.830002,63152400,122.905254,106.7410523,117.9276669,129.1142814,Increasing
2015-02-18,127.629997,128.779999,127.449997,128.720001,44891700,123.760965,107.842423,118.9403335,130.0382439,Increasing
2015-02-19,128.479996,129.029999,128.330002,128.449997,37362400,123.501363,108.8942449,119.8891668,130.8840887,Decreasing
2015-02-20,128.619995,129.5,128.050003,129.5,48948400,124.510914,109.7854494,120.7635001,131.7415509,Increasing
   *)
  let traces_json =
    `List
      [
        `Assoc
          [
            ("type", `String "scatter");
            ("mode", `String "lines");
            ("name", `String "Course 1");
            (* TODO: add translation here *)
            ( "x",
              `List
                [
                  `String "2015-02-17";
                  `String "2015-02-18";
                  `String "2015-02-19";
                  `String "2015-02-20";
                ] );
            ("y", `List [ `String "1"; `String "2"; `String "3"; `String "1" ]);
            ("line", `Assoc [ ("color", `String "#17BECF") ]);
          ];
      ]
  in
  let layout_json =
    `Assoc
      [
        ("title", `Assoc [ ("text", `String "Rating Graph") ]);
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
                              ("count", `Int 1);
                              ("label", `String "1m");
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
      ]
  in
  let config_json = `Assoc [] in
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

let profile (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list)
    (simple_results : Db.Types.SimpleResult.t list)
    (runner_info : Db.Types.RunnerInfo.t) (medals : Db.Types.Medals.t) =
  let _ = t in
  (* TODO: add hovers with description *)
  (* TODO: add totals to page *)
  let sorted_ratings_per_course =
    [ "1"; "2"; "3"; "D" ]
    |> List.map ~f:(fun course_id ->
           let latest_rating =
             List.filter ratings ~f:(fun rating ->
                 String.equal rating.course_id course_id)
             |> List.sort ~compare:(fun r1 r2 ->
                    String.compare r1.event_date r2.event_date)
           in
           (course_id, latest_rating))
  in
  let rating_info =
    List.map sorted_ratings_per_course ~f:(fun (course_id, ratings) ->
        let num_events =
          List.filter simple_results ~f:(fun r ->
              String.equal r.course_id course_id)
          |> List.length
        in
        rating_section t course_id (List.last ratings) num_events)
  in
  div
    [ class_ "profile-container page-small box" ]
    [
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
            (* TODO: all rating info for each course - with history *)
            (* TODO: all medals *)
            (* TODO: all events participated with position and course *)
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
          (* TODO: fix how rating-info is shown on the page *)
        ];
      div [ class_ "rating-info" ] rating_info;
      rating_graph sorted_ratings_per_course;
    ]

let page (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list)
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
            [ profile t ratings simple_results runner_info medals ];
        ];
    ]
