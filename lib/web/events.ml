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
          path_attr href Static.Assets.Css.events_css;
        ];
    ]

let container (t : Page_settings.t) =
  (* TODO: calculate this *)
  let current_year = 2026 in

  let year_buttons =
    List.init 10 ~f:(fun i ->
        let year = current_year - i in
        button [] [ txt "%d" year ])
  in
  let league_buttons =
    [ t.translations.all_leagues; "Vilniaus Ketvirtadieniai" ]
    |> List.map ~f:(fun name -> button [] [ txt "%s" name ])
  in
  div
    [ class_ "profile-container page-small box" ]
    [
      div
        [ class_ "box-contents" ]
        [ h1 [ class_ "page-title" ] [ txt "%s" t.translations.events ] ];
      div [ class_ "angle-content" ] year_buttons;
      div [ class_ "angle-content" ] league_buttons;
    ]

let page (t : Page_settings.t) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body [] [ Header.elements t; div [ id "main-wrap" ] [ container t ] ];
    ]
