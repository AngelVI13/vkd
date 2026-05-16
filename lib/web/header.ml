open! Core
open Dream_html
open HTML

let nav_section text path =
  section [] [ a [ path_attr href path ] [ span [] [ txt text ] ] ]

let elements () =
  header
    [ id "top" ]
    [
      div
        [ class_ "site-title-nav" ]
        [
          a
            [ class_ "site-title"; path_attr href Paths.index ]
            [
              div
                [ class_ "site-icon" ]
                [ img [ path_attr src Static.Assets.Images.logo5_png ] ];
              div [ class_ "site-name" ] [ txt "O-Stats" ];
            ];
          nav
            (* TODO: implement the hover effect *)
            [ id "topnav"; class_ "hover" ]
            [
              nav_section "Events" Paths.index;
              nav_section "Leagues" Paths.index;
              nav_section "Runners" Paths.index;
            ];
        ];
      div [ class_ "site-buttons" ] [];
    ]
