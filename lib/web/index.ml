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

let ratings_table (t : Page_settings.t) =
  let _ = t in
  null
    [
      div
        [ class_ "box__top" ]
        [
          (* TODO: put filter buttons here *)
          a
            [ path_attr href Paths.user_w_scope t.translations.lang ]
            [ txt "User" ];
        ];
      table
        [ class_ "slist slist-pad slist-invert slist-leaderboard" ]
        [
          tbody
            [ class_ "infinite-scroll" ]
            [
              tr []
                [
                  td []
                    [
                      span
                        [
                          class_ "trophy perf top1 lb__trophy trophy--small";
                          title_ "Blitz Chamption!";
                        ]
                        [
                          img
                            [
                              src
                                "https://lichess1.org/assets/hashed/gold-cup-2.e1e2ac3f.png";
                              style_ "height: 40px;";
                            ];
                        ];
                    ];
                  td []
                    [
                      a
                        [
                          class_ "offline user-link ulpt";
                          path_attr href Paths.index;
                        ]
                        [ txt "asdert9" ];
                    ];
                  td [] [ txt "3043" ];
                  td [] [ txt "10" ];
                ];
            ];
        ];
    ]

let prev_next_event (t : Page_settings.t) =
  let _ = t in
  div
    [ class_ "prev-next-events" ]
    [
      div
        [ class_ "page-small box" ]
        [
          div [ class_ "prev-event" ] [ txt "Past Event" ];
          div [ class_ "next-event" ] [ txt "Next Event" ];
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
          div
            [ id "main-wrap" ]
            [
              prev_next_event t;
              main [ class_ "page-small box" ] [ ratings_table t ];
            ];
        ];
    ]
