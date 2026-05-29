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
      select
        [ class_ "op-hover"; name "language"; Hx.get ""; Hx.target "body" ]
        options;
    ]

let dark_mode_select (dark_mode : string) =
  div
    [ class_ "dark-mode-toggle" ]
    [
      input
        [
          type_ "range";
          step "1";
          min "0";
          max "1";
          class_ "toggle-slider op-hover";
          name "dark-mode";
          value "%s" dark_mode;
          id "toggle";
          Hx.get "";
          Hx.trigger "change";
        ];
      img [ path_attr src Static.Assets.Images.night_mode_png ];
    ]

let elements (t : Page_settings.t) =
  header
    [ id "top" ]
    [
      div
        [ class_ "site-title-nav" ]
        [
          div
            [ class_ "nav-menu-container op-hover" ]
            [
              img
                [
                  class_ "nav-menu";
                  id "nav-menu-img";
                  tabindex 0;
                  path_attr src Static.Assets.Images.menu_png;
                ];
            ];
          a
            [
              class_ "site-title";
              path_attr href Paths.index_w_scope t.translations.lang;
            ]
            [
              div
                [ class_ "site-icon" ]
                (* [ img [ path_attr src Static.Assets.Images.logo5_png ] ]; *)
                [ img [ path_attr src Static.Assets.Images.vkd_logo_png ] ];
              div [ class_ "site-name" ] [ txt "O-Stats" ];
            ];
          div [ class_ "overlay" ] [];
          nav
            [ id "topnav"; class_ "hover" ]
            [
              nav_section t.translations.events Paths.index;
              nav_section t.translations.leagues Paths.index;
              nav_section t.translations.runners Paths.index;
            ];
        ];
      div
        [ class_ "site-buttons" ]
        [
          dark_mode_select t.dark_mode;
          lang_select
            ~selected_lang:(Localization.language_of_abbrev t.translations.lang);
        ];
    ]
