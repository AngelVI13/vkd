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
    ]

let profile (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list) =
  let _ = t in
  let rating_info =
    [ "1"; "2"; "3"; "D" ]
    |> List.map ~f:(fun course_id ->
           let latest_rating =
             List.filter ratings ~f:(fun rating ->
                 String.equal rating.course_id course_id)
             |> List.sort ~compare:(fun r1 r2 ->
                    -1 * String.compare r1.event_date r2.event_date)
             |> List.hd
           in
           let latest_rating =
             match latest_rating with
             | None -> "?"
             | Some r -> sprintf "%.0f" r.rating
           in
           (course_id, latest_rating))
    |> List.map ~f:(fun (course_id, rating) ->
           let course_icon =
             match course_id with
             | "1" -> Icons.course1
             | "2" -> Icons.course2
             | "3" -> Icons.course3
             | "D" -> Icons.bike_badge
             | _ -> null []
           in
           div
             [ class_ "course-rating" ]
             [
               div [ class_ "course-id" ] [ course_icon; txt "%s" course_id ];
               div [ class_ "rating" ] [ txt "%s" rating ];
             ])
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
              h1 [ class_ "runner-name" ] [ txt "%s" "Angel" ];
              p [ class_ "runner-club" ] [ txt "%s" "Bet koks" ];
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
                [ Icons.medal; div [] [ txt "13" ] ];
              div
                [ class_ "medal-num silver" ]
                [ Icons.medal; div [] [ txt "2" ] ];
              div
                [ class_ "medal-num bronze" ]
                [ Icons.medal; div [] [ txt "7" ] ];
            ];
          (* TODO: fix how rating-info is shown on the page *)
        ];
      div [ class_ "rating-info" ] rating_info;
    ]

let page (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [ Header.elements t; div [ id "main-wrap" ] [ profile t ratings ] ];
    ]
