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

let details_with_img (ev : Db.EventInfoExtra.t) (date : string)
    (location : string) (opacity : float) =
  [
    span
      [ class_ "event-top" ]
      [
        img
          [
            class_ "event-img";
            style_ "--opacity: %.2f" opacity;
            (if Option.is_some ev.thumbnail then
               src "data:image/jpg;base64,%s" (Option.value_exn ev.thumbnail)
             else path_attr src Static.Assets.Images.map_placeholder_jpg);
          ];
        time
          [ class_ "over-img"; datetime "%s" date; title_ "%s" date ]
          [ txt "%s" date ];
        span [ class_ "event-loc over-img pos-bottom" ] [ txt "%s" location ];
      ];
    span
      [ class_ "event-bottom" ]
      [
        h4 [ class_ "event-league" ] [ txt "%s" ev.league_name ];
        (if Option.is_some ev.map_info then
           p
             [ class_ "event-map-info" ]
             [ txt "%s" (Option.value_exn ev.map_info) ]
         else null []);
      ];
  ]

(* TODO: whats the lithuanian equivalent of TBA (to be announced) *)
let event_details_placeholder = "TBA"

let event_card (t : Page_settings.t) (ev : Db.EventInfoExtra.t) =
  let today_date = Utils.today_string () in
  let date = ev.event_date in
  let location =
    match ev.official_location with None -> ev.location | Some l -> l
  in
  let is_past_event =
    String.(ev.event_date = event_details_placeholder)
    || String.(ev.event_date < today_date)
  in
  let conditional_class =
    if is_past_event then "past-event" else "border-highlight"
  in
  let opacity = if is_past_event then 0.3 else 1.0 in
  let card =
    a
      [
        class_ "event-card event-details-lnk %s" conditional_class;
        (* TODO: this should be the link to the event / event results *)
        path_attr href Paths.index_w_scope t.translations.lang;
      ]
      (details_with_img ev date location opacity)
  in
  card

let event_carousel (t : Page_settings.t) (events : Db.EventInfoExtra.t list) =
  let default =
    Db.EventInfoExtra.Fields.create ~event_id:0 ~league_id:0 ~league_name:""
      ~event_nr:0 ~event_date:event_details_placeholder
      ~location:event_details_placeholder ~event_link:None ~thumbnail:None
      ~map_info:(Some event_details_placeholder) ~official_location:None
      ~links:[]
  in

  (* TODO: remove after testing *)
  (* let events = List.take events 3 in *)
  let today_date = Utils.today_string () in
  let future_events =
    List.filter events ~f:(fun e -> String.(e.event_date > today_date))
  in
  let events =
    (* Add default empty event if we don't have any future events *)
    if List.length future_events = 0 then default :: events else events
  in
  div
    [ class_ "carousel__track page-small" ]
    (List.map events ~f:(fun e -> event_card t e))

let page (t : Page_settings.t) (events : Db.EventInfoExtra.t list) =
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
              event_carousel t events;
              main [ class_ "page-small box" ] [ ratings_table t ];
            ];
        ];
    ]
