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

let details_for_existing_event__ (ev : Db.EventInfoExtra.t) =
  let date = Utils.format_time_as_date ev.event_date in
  let location =
    match ev.official_location with None -> ev.location | Some l -> l
  in
  [
    div [ class_ "event-date" ] [ span [] [ txt "%s" date ] ];
    div [ class_ "event-location" ] [ span [] [ txt "%s" location ] ];
    (if Option.is_some ev.thumbnail then
       div
         [ class_ "event-img" ]
         [
           img
             [ src "data:image/jpg;base64,%s" (Option.value_exn ev.thumbnail) ];
         ]
     else null []);
    (if Option.is_some ev.map_info then
       div
         [ class_ "event-map-info" ]
         [ span [] [ txt "%s" (Option.value_exn ev.map_info) ] ]
     else null []);
  ]

let details_with_img (ev : Db.EventInfoExtra.t) (date : string)
    (location : string) =
  let thumbnail = Option.value_exn ev.thumbnail in
  [
    span
      [ class_ "event-top" ]
      [
        img [ class_ "event-img"; src "data:image/jpg;base64,%s" thumbnail ];
        time
          [ class_ "over-img"; datetime "%s" date; title_ "%s" date ]
          [ txt "%s" date ];
        span [ class_ "event-loc over-img pos-bottom" ] [ txt "%s" location ];
      ];
    (if Option.is_some ev.map_info then
       span
         [ class_ "event-bottom" ]
         [
           h3
             [ class_ "event-map-info" ]
             [ txt "%s" (Option.value_exn ev.map_info) ];
         ]
     else null []
       (* div [ class_ "event-date" ] [ span [] [ txt "%s" date ] ]; *)
       (* div [ class_ "event-location" ] [ span [] [ txt "%s" location ] ]; *)
       (* div *)
       (*   [ class_ "event-img" ] *)
       (*   [ img [ src "data:image/jpg;base64,%s" thumbnail ] ]; *)
       (* (if Option.is_some ev.map_info then *)
       (*    div *)
       (*      [ class_ "event-map-info" ] *)
       (*      [ span [] [ txt "%s" (Option.value_exn ev.map_info) ] ] *)
       (*  else null []); *));
  ]

let details_no_img (ev : Db.EventInfoExtra.t) (date : string)
    (location : string) =
  [
    div [ class_ "event-date" ] [ span [] [ txt "%s" date ] ];
    div [ class_ "event-location" ] [ span [] [ txt "%s" location ] ];
    (if Option.is_some ev.map_info then
       div
         [ class_ "event-map-info" ]
         [ span [] [ txt "%s" (Option.value_exn ev.map_info) ] ]
     else null []);
  ]

let details_for_existing_event (t : Page_settings.t) (ev : Db.EventInfoExtra.t)
    =
  let date = Utils.format_time_as_date ev.event_date in
  let location =
    match ev.official_location with None -> ev.location | Some l -> l
  in
  let details =
    match ev.thumbnail with
    | None -> details_no_img ev date location
    | Some _ -> details_with_img ev date location
  in
  [
    a
      [
        class_ "event-details-lnk";
        path_attr href Paths.index_w_scope t.translations.lang;
      ]
      details;
  ]

let event_details (t : Page_settings.t) (event : Db.EventInfoExtra.t option) =
  match event with None -> [] | Some ev -> details_for_existing_event t ev

let event_info ~(event_type : string) (event : Db.EventInfoExtra.t option)
    (t : Page_settings.t) =
  (* TODO: links to event page & more info to event (atleast for bigger screens) *)
  div
    [ class_ "event-container" ]
    [
      div [ class_ "event-type" ] [ span [] [ txt "%s" event_type ] ];
      div [ class_ "event-details" ] (event_details t event);
    ]

let prev_next_event (t : Page_settings.t)
    (events : Db.EventInfoExtra.t option * Db.EventInfoExtra.t option) =
  let event_before, event_after = events in
  div
    [ class_ "prev-next-events" ]
    [
      div
        [ class_ "page-small box" ]
        [
          div
            [ class_ "prev-event" ]
            [ event_info ~event_type:t.translations.prev_event event_before t ];
          div
            [ class_ "next-event" ]
            [ event_info ~event_type:t.translations.next_event event_after t ];
        ];
    ]

let page (t : Page_settings.t)
    (events : Db.EventInfoExtra.t option * Db.EventInfoExtra.t option) =
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
              prev_next_event t events;
              main [ class_ "page-small box" ] [ ratings_table t ];
            ];
        ];
    ]
