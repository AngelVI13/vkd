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
          path_attr href Static.Assets.Css.index_css;
        ];
    ]

let page (t : Page_settings.t) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [
          Header.elements t;
          h1 [] [ txt "%s" t.translations.runner ];
          a
            [ path_attr href Paths.index_w_scope t.translations.lang ]
            [ txt "Home" ];
        ];
    ]
