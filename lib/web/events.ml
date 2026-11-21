open Core
open Dream_html
open HTML

let head_elems (t : Page_settings.t) =
  Common.head_elems t.dark_mode
  @ [
      link
        [
          rel "stylesheet";
          type_ "text/css";
          path_attr href Static.Assets.Css.events_css;
        ];
    ]

let year_buttons (t : Page_settings.t) ~(year_selected : string)
    ~(league_selected : string) =
  let current_year =
    let now = Time_ns_unix.now () in
    Time_ns_unix.to_date ~zone:Timezone.utc now |> Date.year
  in

  (* TODO: i'm not sure its worth it to ask the DB whats the earliest year -
       so hardcoding 10 here seems ok *)
  List.init 10 ~f:(fun i ->
      let year = current_year - i in
      let is_selected = String.equal year_selected (Int.to_string year) in
      button
        [ (if is_selected then class_ "selected" else null_) ]
        [
          a
            [
              (if not is_selected then
                 path_attr href Paths.events_w_scope_w_params
                   t.translations.lang (Int.to_string year) league_selected
               else null_);
            ]
            [ txt "%d" year ];
        ])

let league_buttons (t : Page_settings.t) ~(year_selected : string)
    ~(league_selected : string) =
  [ t.translations.all_leagues; Dbsportas.League.LeagueInfo.main_league_name ]
  |> List.map ~f:(fun name ->
         let is_selected =
           (* NOTE: this is a very stupid way to check this but i can't
                  think of a better one. It sets VKD as selected if user
                  selected VKD, if not it sets the other button as selected *)
           String.equal league_selected
             Dbsportas.League.LeagueInfo.main_league_name
           && String.equal name league_selected
           || (not
                 (String.equal league_selected
                    Dbsportas.League.LeagueInfo.main_league_name))
              && not
                   (String.equal name
                      Dbsportas.League.LeagueInfo.main_league_name)
         in
         button
           [ (if is_selected then class_ "selected" else null_) ]
           [
             a
               [
                 (if not is_selected then
                    path_attr href Paths.events_w_scope_w_params
                      t.translations.lang year_selected name
                  else null_);
               ]
               [ txt "%s" name ];
           ])

let event_card (t : Page_settings.t) (event : Db.Types.EventInfoExtra.t) =
  (* TODO: optimize this that when the client receives the page it
              should asyncronously request each image. This shuold make the
              page load fast and hopefully also allow the browser to cache the
              replies so that the user will not have to dowload all images
              every time *)
  (* TODO: do the same for the index page images *)
  let date = event.event_date in
  let thumbnail = event.thumbnail in
  let card =
    a
      [
        class_ "event-card event-details-lnk";
        (* TODO: this should be the link to the event / event results *)
        path_attr href Paths.index_w_scope t.translations.lang;
      ]
      [
        span
          [ class_ "event-top" ]
          [
            img
              [
                class_ "event-img";
                (if Option.is_some thumbnail then
                   src "data:image/jpg;base64,%s" (Option.value_exn thumbnail)
                 else path_attr src Static.Assets.Images.map_placeholder_jpg);
              ];
            time
              [
                class_ "event-date over-img";
                datetime "%s" date;
                title_ "%s" date;
              ]
              [ txt "%s" date ];
            span
              [ class_ "event-location over-img pos-bottom" ]
              [ txt "%s" event.league_name ];
          ];
        span
          [ class_ "event-bottom" ]
          [
            h3 [ class_ "event-league" ] [ txt "%s" event.location ];
            (if Option.is_some event.map_info then
               p
                 [ class_ "event-map-info" ]
                 [ txt "%s" (Option.value_exn event.map_info) ]
             else null []);
          ];
      ]
  in
  card

let event_rows (t : Page_settings.t) (events : Db.Types.EventInfoExtra.t list) =
  List.map events ~f:(fun event -> event_card t event)

let container (t : Page_settings.t) ~(year_selected : string)
    ~(league_selected : string) (events : Db.Types.EventInfoExtra.t list) =
  let year_buttons = year_buttons t ~year_selected ~league_selected in
  let league_buttons = league_buttons t ~year_selected ~league_selected in
  let event_rows = event_rows t events in
  div
    [ class_ "profile-container page-small box" ]
    [
      div
        [ class_ "box-contents" ]
        [ h1 [ class_ "page-title" ] [ txt "%s" t.translations.events ] ];
      div [ class_ "angle-content year-btns" ] year_buttons;
      div [ class_ "angle-content league-btns" ] league_buttons;
      div [ class_ "angle-content event-rows" ] event_rows;
    ]

let page (t : Page_settings.t) ~(year_selected : string)
    ~(league_selected : string) (events : Db.Types.EventInfoExtra.t list) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [
          Header.elements t;
          div
            [ id "main-wrap" ]
            [ container t ~year_selected ~league_selected events ];
        ];
    ]
