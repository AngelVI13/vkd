open! Core
open Dream_html
open HTML

let nav_section text path =
  section [] [ a [ path_attr href path ] [ span [] [ txt "%s" text ] ] ]

let lang_select () =
  let options =
    List.map Localization.all_of_language ~f:(fun l ->
        let abbrev = Localization.language_to_abbrev l in
        option [ value "%s" abbrev ] "%s" (String.uppercase abbrev))
  in
  select [ name "language" ] options

let lang_select_v2 ~(selected : Localization.language) =
  let options =
    List.map Localization.all_of_language ~f:(fun l ->
        let abbrev = Localization.language_to_abbrev l in
        let flag = Localization.language_flag l in

        let cls = "flag" in
        let cls =
          if Localization.equal_language selected l then
            sprintf "%s selected" cls
          else cls
        in
        div
          [ class_ "%s" cls ]
          [
            a
              [ path_attr href Paths.index_w_scope abbrev ]
              [
                img [ path_attr src flag ];
                span [] [ txt "%s" (String.uppercase abbrev) ];
              ];
          ])
  in
  div [ class_ "language-select" ] [ div [ class_ "select" ] options ]

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
        (* TODO: provide this from above *)
        [ lang_select_v2 ~selected:Localization.English ];
    ]
