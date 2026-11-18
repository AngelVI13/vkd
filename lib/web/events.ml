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

let container (t : Page_settings.t) ~(year_selected : string)
    ~(league_selected : string) (events : Db.Types.EventInfoExtra.t list) =
  let current_year =
    let now = Time_ns_unix.now () in
    Time_ns_unix.to_date ~zone:Timezone.utc now |> Date.year
  in

  let year_buttons =
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
  in
  let league_buttons =
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
  in

  let event_rows =
    events
    |> List.map ~f:(fun event ->
           div [] [ txt "%s %s" event.event_date event.location ])
  in
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
