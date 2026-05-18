open! Core
open Dream_html
open HTML

let nav_section text path =
  section [] [ a [ path_attr href path ] [ span [] [ txt "%s" text ] ] ]

let lang_select ~(selected_lang : Localization.language) =
  let flag = Localization.language_flag selected_lang in
  let options =
    List.map Localization.all_of_language ~f:(fun l ->
        let abbrev = Localization.language_to_abbrev l in
        let selected_node =
          if Localization.equal_language l selected_lang then selected
          else null_
        in
        option
          [ value "%s" abbrev; selected_node ]
          "%s" (String.uppercase abbrev))
  in
  div
    [ class_ "language-select" ]
    [
      img [ path_attr src flag ];
      select [ name "language"; Hx.get ""; Hx.target "body" ] options;
    ]

let elements (t : Localization.translations) =
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
            [ id "topnav"; class_ "hover" ]
            [
              nav_section t.events Paths.index;
              nav_section t.leagues Paths.index;
              nav_section t.runners Paths.index;
            ];
        ];
      div
        [ class_ "site-buttons" ]
        [ lang_select ~selected_lang:(Localization.language_of_abbrev t.lang) ];
    ]
