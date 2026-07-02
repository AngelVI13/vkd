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
          path_attr href Static.Assets.Css.index_css;
        ];
    ]

let rating_row ~(t : Page_settings.t) (rating : Glicko2.Rating.Info.t) =
  let is_uncertain = Float.(rating.rd >= 100.) in
  let position =
    match rating.position with None -> "" | Some p -> Int.to_string p
  in
  let extra_classes = if is_uncertain then "help" else "" in
  tr []
    [
      td [ class_ "position" ] [ txt "%s" position ];
      td []
        [
          span
            [ class_ "runner-info" ]
            [
              a
                [
                  class_ "runner-name";
                  path_attr href Paths.user_w_scope t.translations.lang
                    rating.runner_id;
                ]
                [ txt "%s" rating.runner_name ];
              p
                [ class_ "runner-club" ]
                [ txt "%s" (String.strip rating.runner_club) ];
            ];
        ];
      td
        [
          class_ "rating %s" extra_classes;
          (if is_uncertain then title_ "%s" t.translations.rating_uncertainty
           else null_);
          (if is_uncertain then
             style_
               "text-decoration: underline dashed; text-underline-offset: 3px;"
           else null_);
        ]
        [
          txt "%.0f" rating.rating;
          (if is_uncertain then span [ class_ "uncertain" ] [ txt "?" ]
           else null []);
        ];
      td
        [ class_ "rating-diff" ]
        [
          span
            [
              (if Float.(rating.rating_diff > 0.0) then class_ "icon good"
               else if Float.(rating.rating_diff < 0.0) then class_ "icon bad"
               else class_ "icon line");
            ]
            [ txt "%.0f" rating.rating_diff ];
        ];
      td [] [ txt "%s" rating.event_date ];
    ]

let rating_rows ?(page_num : int = 1) (t : Page_settings.t)
    (ratings : Glicko2.Rating.Info.t list) =
  let rows = List.map ratings ~f:(rating_row ~t) in
  let rows =
    if List.length ratings < Settings.ratings_page_size then rows
    else
      match List.last rows with
      | None -> rows
      | Some tl ->
          let rows = List.drop_last_exn rows in
          let last = tl +@ Hx.trigger "intersect once" in
          let last = last +@ Hx.swap "afterend" in
          let last =
            last
            +@ path_attr Hx.get Paths.rating_table_w_scope_w_page
                 t.translations.lang (page_num + 1)
            +@ Hx.include_ "#filter-form,#rating-search"
          in
          rows @ [ last ]
  in
  rows

let ratings_table ?(page_num : int = 1) (t : Page_settings.t)
    (ratings : Glicko2.Rating.Info.t list) =
  let rows = rating_rows ~page_num t ratings in
  div
    [ class_ "rating-table-container" ]
    [
      table
        [
          class_ "slist slist-pad slist-invert slist-leaderboard";
          id "rating-table";
        ]
        [
          thead []
            [
              tr []
                [
                  th [ class_ "position" ] [ txt "#" ];
                  th [] [ txt "%s" t.translations.name ];
                  th
                    [
                      class_ "help";
                      title_ "%s" t.translations.rating_description;
                    ]
                    [ txt "%s" t.translations.rating ];
                  th
                    [
                      class_ "help";
                      title_ "%s" t.translations.rating_change_description;
                    ]
                    [ txt "%s" t.translations.change ];
                  th
                    [
                      class_ "help";
                      title_ "%s" t.translations.last_event_description;
                    ]
                    [ txt "%s" t.translations.last_event ];
                ];
            ];
          tbody [ id "rating-rows"; class_ "infinite-scroll" ] rows;
        ];
    ]

type ratingCourse = Course1 | Course2 | Course3 | CourseD
[@@deriving enumerate, eq]

let show_ratingCourse = function
  | Course1 -> "1"
  | Course2 -> "2"
  | Course3 -> "3"
  | CourseD -> "D"

let ratingCourse_of_string = function
  | "1" -> Course1
  | "2" -> Course2
  | "3" -> Course3
  | "D" -> CourseD
  | _ -> assert false

let ratingCourse_eq (rating_course : ratingCourse) (course_id : string) : bool =
  String.equal (show_ratingCourse rating_course) course_id

type ratingGroup = GroupAll | GroupMen | GroupWomen [@@deriving enumerate, eq]

let show_ratingGroup = function
  | GroupAll -> "All"
  | GroupMen -> "Men"
  | GroupWomen -> "Women"

let ratingGroup_of_string = function
  | "All" -> GroupAll
  | "Women" -> GroupWomen
  | "Men" -> GroupMen
  | _ -> assert false

let ratingGroup_eq (group : ratingGroup) (runner_gender : string) : bool =
  match group with
  | GroupAll -> true
  | GroupWomen -> String.equal runner_gender "M"
  | GroupMen -> String.equal runner_gender "V"

let ratings_header ?(selected_course : ratingCourse = Course1)
    ?(selected_group : ratingGroup = GroupAll) (t : Page_settings.t) =
  let course_options =
    List.map all_of_ratingCourse ~f:(fun c ->
        let course_string = show_ratingCourse c in
        let selected_node =
          if equal_ratingCourse c selected_course then selected else null_
        in
        option [ value "%s" course_string; selected_node ] "%s" course_string)
  in
  let group_options =
    List.map all_of_ratingGroup ~f:(fun g ->
        let selected_node =
          if equal_ratingGroup g selected_group then selected else null_
        in
        let option_txt =
          match g with
          | GroupAll -> t.translations.all
          | GroupMen -> t.translations.men
          | GroupWomen -> t.translations.women
        in
        option
          [ value "%s" (show_ratingGroup g); selected_node ]
          "%s" option_txt)
  in
  let select_form =
    form
      [
        class_ "rating-select-filters";
        id "filter-form";
        path_attr Hx.get Paths.rating_table_w_scope t.translations.lang;
        Hx.target "#rating-rows";
        Hx.swap "innerHTML";
        Hx.include_ "#rating-search";
        Hx.trigger "change";
        Hx.indicator ".search-container";
      ]
      [
        label [] [ txt "%s" t.translations.course ];
        select [ class_ "op-hover"; name "course-select" ] course_options;
        label [] [ txt "%s" t.translations.group ];
        select [ class_ "op-hover"; name "group-select" ] group_options;
      ]
  in
  let search_input =
    input
      [
        class_ "form-control";
        type_ "search";
        name "rating-search";
        id "rating-search";
        placeholder "%s" t.translations.type_to_search_runner;
        Hx.target "#rating-rows";
        Hx.swap "innerHTML";
        Hx.include_ "#filter-form";
        path_attr Hx.post Paths.rating_table_w_scope t.translations.lang;
        Hx.trigger "input changed delay:500ms";
        Hx.indicator ".search-container";
      ]
  in
  div
    [ class_ "box__top" ]
    [
      div
        [
          class_ "rating-table-title help";
          title_ "%s" t.translations.rating_description;
        ]
        [
          span []
            [
              div
                [ class_ "rating-table-txt" ]
                [ txt "%s" t.translations.performance_ratings ];
              div
                [ class_ "rating-table-desc" ]
                [ txt "%s" Dbsportas.League.LeagueInfo.main_league_name ];
            ];
        ];
      div
        [ class_ "rating-table-filters" ]
        [
          select_form;
          div [ class_ "search-container search-icon" ] [ search_input ];
        ];
    ]

let ratings_section (t : Page_settings.t) (ratings : Glicko2.Rating.Info.t list)
    =
  main [ class_ "page-small box" ] [ ratings_header t; ratings_table t ratings ]

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

  let today_date = Utils.today_string () in
  let future_events =
    List.filter events ~f:(fun e -> String.(e.event_date >= today_date))
  in
  let events =
    (* Add default empty event if we don't have any future events *)
    if List.length future_events = 0 then default :: events else events
  in
  div
    [ class_ "carousel__track page-small" ]
    (List.map events ~f:(fun e -> event_card t e))

let powered_by (t : Page_settings.t) =
  let _ = t in
  div
    [ class_ "powered-by" ]
    [
      div [ class_ "powered-by-txt" ] [ txt "%s:" t.translations.powered_by ];
      div
        [ class_ "powered-by-source" ]
        [
          a
            [ href "https://dbsportas.lt" ]
            [
              img
                [
                  class_ "powered-by-img dbsportas";
                  path_attr src Static.Assets.Images.db_sport_svg;
                ];
            ];
        ];
      div
        [ class_ "powered-by-source" ]
        [
          a
            [ href "https://vilniausketvirtadieniai.lt" ]
            [
              img
                [
                  class_ "powered-by-img vkd";
                  path_attr src Static.Assets.Images.vk_logo_png;
                ];
            ];
        ];
    ]

let page (t : Page_settings.t) (events : Db.EventInfoExtra.t list)
    (ratings : Glicko2.Rating.Info.t list) =
  html
    [ lang "en" ]
    [
      head [] (head_elems t);
      body []
        [
          Header.elements t;
          div
            [ id "main-wrap" ]
            [ powered_by t; event_carousel t events; ratings_section t ratings ];
        ];
    ]
