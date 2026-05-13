open! Core
open Dream_html
open HTML

let page (module T : Localization.TRANSLATIONS) =
  html
    [ lang "en" ]
    [
      head [] [];
      body []
        [
          h1 [] [ txt "%s" T.runner ];
          a [ path_attr href Paths.index_w_scope T.lang ] [ txt "Home" ];
        ];
    ]
