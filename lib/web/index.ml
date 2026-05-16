open! Core
open Dream_html
open HTML

let head_elems () =
  Common.head_elems
  @ [
      link
        [
          rel "stylesheet";
          type_ "text/css";
          path_attr href Static.Assets.Css.index_css;
        ];
    ]

let page (t : Localization.translations) =
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [
          Header.elements t;
          h1 [] [ txt "%s" t.runner ];
          a [ path_attr href Paths.user_w_scope t.lang ] [ txt "User" ];
        ];
    ]
