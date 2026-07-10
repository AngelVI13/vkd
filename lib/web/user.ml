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

let xmlns_xlink fmt = string_attr "xmlns:xlink" fmt
let xml_space fmt = string_attr "xml:space" fmt
let version_ fmt = string_attr "version" fmt

let medal () =
  let open SVG in
  svg
    [
      fill "#000000";
      height "800px";
      width "800px";
      version_ "1.1";
      id "Capa_1";
      xmlns;
      xmlns_xlink "http://www.w3.org/1999/xlink";
      viewbox ~min_x:0 ~min_y:0 ~width:296 ~height:296;
      xml_space "preserve";
    ]
    [
      path
        [
          d
            "M191.27,84.676l24.919-21.389c4.182-3.572,7.52-11.037,7.52-16.537v-37c0-5.5-4.167-9.75-9.667-9.75h-58.333v76.689 \
             C168.709,77.51,180.064,80.221,191.27,84.676z";
        ]
        [];
      path
        [
          d
            "M140.709,0H82.042c-5.5,0-10.333,4.25-10.333,9.75v37c0,5.5,3.588,12.922,7.77,16.494l24.928,21.428 \
             c11.508-4.574,24.302-7.307,36.302-8.045V0z";
        ]
        [];
      path
        [
          d
            "M148.041,91.416c-56.516,0-102.332,45.816-102.332,102.334s45.816,102.334,102.332,102.334 \
             c56.518,0,102.334-45.816,102.334-102.334S204.559,91.416,148.041,91.416z \
             M148.041,275.377c-45.008,0-81.625-36.619-81.625-81.627 \
             c0-45.01,36.617-81.627,81.625-81.627c45.01,0,81.627,36.617,81.627,81.627C229.668,238.758,193.051,275.377,148.041,275.377z";
        ]
        [];
      path
        [
          d
            "M148.041,127.123c-36.736,0-66.625,29.889-66.625,66.627s29.889,66.627,66.625,66.627 \
             c36.738,0,66.627-29.889,66.627-66.627S184.779,127.123,148.041,127.123z";
        ]
        [];
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
             | None -> ""
             | Some r -> sprintf "%.0f" r.rating
           in
           (course_id, latest_rating))
    |> List.map ~f:(fun (course_id, rating) ->
           div
             [ class_ "course-rating" ]
             [
               div [ class_ "course-id" ] [ txt "%s" course_id ];
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
              div [ class_ "medal-num gold" ] [ medal (); div [] [ txt "13" ] ];
              div [ class_ "medal-num silver" ] [ medal (); div [] [ txt "2" ] ];
              div [ class_ "medal-num bronze" ] [ medal (); div [] [ txt "7" ] ];
            ];
          (* TODO: fix how rating-info is shown on the page *)
          div [ class_ "rating-info" ] rating_info;
        ];
    ]

let page (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [ Header.elements t; div [ id "main-wrap" ] [ profile t ratings ] ];
    ]
