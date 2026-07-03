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

let profile (t : Page_settings.t) =
  let _ = t in
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
        ];
    ]

let page (t : Page_settings.t) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body [] [ Header.elements t; div [ id "main-wrap" ] [ profile t ] ];
    ]
