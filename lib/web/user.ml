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
  div
    [ class_ "profile-container page-small box" ]
    [ txt "%s: %s" t.translations.runner "Angel" ]

let page (t : Page_settings.t) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body [] [ Header.elements t; div [ id "main-wrap" ] [ profile t ] ];
    ]
