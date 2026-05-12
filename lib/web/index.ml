open! Core
open Dream_html
open HTML

let head_elems () =
  [
    meta [ http_equiv `content_type; content "text/html; charset=UTF-8" ];
    meta [ charset "UTF-8" ];
    meta [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
    link
      [
        rel "icon";
        type_ "image/png";
        path_attr href Static.Assets.Images.logo_png;
      ];
    title [] "O-Stats";
    (* NOTE: htmx version 2.0.7 *)
    script [ path_attr src Static.Assets.Js.Htmx.Dist.htmx_min_js ] "";
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.common_css;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.header_css;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.index_css;
      ];
  ]

let top_header () =
  header
    [ id "top" ]
    [
      div
        [ class_ "site-title-nav" ]
        [
          a
            [ class_ "site-title"; path_attr href Paths.index ]
            [
              (* TODO: add icon here as well *)
              div [ class_ "site-name" ] [ txt "O-Stats" ];
            ];
        ];
      div [ class_ "site-buttons" ] [];
    ]

let page (translations : Localization.translations -> string) =
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [ top_header (); h1 [] [ txt "%s" (translations Localization.Runner) ] ];
    ]
