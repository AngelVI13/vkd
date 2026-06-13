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
    (location : string) =
  [
    span
      [ class_ "event-top" ]
      [
        img
          [
            class_ "event-img";
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

let event_card (t : Page_settings.t) (ev : Db.EventInfoExtra.t)
    (opacity : float) =
  let today_date = Utils.today_string () in
  let date = ev.event_date in
  let location =
    match ev.official_location with None -> ev.location | Some l -> l
  in
  let is_past_event =
    (* TODO: don't hardcode the TBA here *)
    String.(ev.event_date = "TBA") || String.(ev.event_date < today_date)
  in
  let past_event_class = if is_past_event then "past-event" else "" in
  let card =
    a
      [
        class_ "event-card event-details-lnk %s" past_event_class;
        style_ "--opacity: %.2f" opacity;
        (* TODO: this should be the link to the event / event results *)
        path_attr href Paths.index_w_scope t.translations.lang;
      ]
      (details_with_img ev date location)
  in
  card

let card_opacities num_cards =
  match num_cards with
  | 5 -> [ 0.25; 0.40; 0.55; 0.7; 1. ]
  | 4 -> [ 0.25; 0.50; 0.7; 1. ]
  | 3 -> [ 0.25; 0.7; 1. ]
  | 2 -> [ 0.7; 1. ]
  | 1 -> [ 1. ]
  | _ -> []

let event_carousel (t : Page_settings.t) (events : Db.EventInfoExtra.t list) =
  let default =
    (* TODO: whats the lithuanian equivalent of TBA (to be announced) *)
    Db.EventInfoExtra.Fields.create ~event_id:0 ~league_id:0 ~league_name:""
      ~event_nr:0 ~event_date:"TBA" ~location:"TBA" ~event_link:None
      ~thumbnail:None ~map_info:(Some "TBA") ~official_location:None ~links:[]
  in
  let events =
    if List.length events < 5 then events @ [ default ] else events
  in
  let opacities = card_opacities (List.length events) in
  div
    [ class_ "carousel__track page-small" ]
    (List.mapi events ~f:(fun idx e ->
         let card_opacity =
           List.nth opacities idx |> Option.value ~default:1.0
         in
         event_card t e card_opacity))

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
