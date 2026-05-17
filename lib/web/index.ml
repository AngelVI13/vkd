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
          div
            [ id "main-wrap" ]
            [
              main
                [ class_ "page-small box" ]
                [
                  div
                    [ class_ "box__top" ]
                    [
                      (* TODO: put filter buttons here *)
                      a
                        [ path_attr href Paths.user_w_scope t.lang ]
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
                                      class_
                                        "trophy perf top1 lb__trophy \
                                         trophy--small";
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
                ];
            ];
        ];
    ]
