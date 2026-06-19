open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* session fields *)
let dark_mode_field = "dark_mode"

(* request fields *)
let translations_field =
  Dream.new_field () ~name:"translations"
    ~show_value:Localization.show_translations

(* TODO: when to trigger the updates of db ? 
         - add_leagues_if_not_exists handle Dbsportas.League.leagues;
         - action_refresh_events_and_results handle; 
         - action_refresh_event_details ~year:(Some "2026") handle;-
       *)

(* TODO: when to invalidate the cache ? Not every db update will have data to update 
   so maybe we can check if we actually sent any data to the DB and if thats the case 
   we can invalidate the cache *)
module State = struct
  type t = {
    filename : string;
    mutable latest_league_events : Db.EventInfoExtra.t list; [@default []]
    mutable all_latest_ratings : Glicko2.Rating.Info.t list; [@default []]
  }
  [@@deriving yojson]

  let save (t : t) = Yojson.Safe.to_file t.filename (yojson_of_t t)

  let load (filename : string) =
    match Yojson.Safe.from_file filename with
    | t -> t_of_yojson t
    | exception exc ->
        printf
          "Failed to load cache from file (%s): %s . Initializing an empty \
           cache..."
          filename (Exn.to_string exc);
        { filename; latest_league_events = []; all_latest_ratings = [] }

  let latest_league_events (t : t) (db : Db.t) =
    if List.length t.latest_league_events > 0 then t.latest_league_events
    else (
      Dream.log "Fetching event data from DB";
      let today = Utils.today_string () in
      let events = Db.latest_league_events db today in
      t.latest_league_events <- events;
      save t;
      events)

  let all_latest_ratings (t : t) (db : Db.t) =
    if List.length t.all_latest_ratings > 0 then t.all_latest_ratings
    else (
      Dream.log "Fetching all latest ratings data from DB";
      let ratings = Db.all_latest_ratings_for_last_year db in
      t.all_latest_ratings <- ratings;
      save t;
      ratings)
end

let handle_index ~(db : Db.t) ~(state : State.t) ~settings request =
  let _ = request in
  let events = State.latest_league_events state db in
  let ratings = State.all_latest_ratings state db in

  let page = Index.page settings events ratings in
  Dream_html.respond page

let handle_user ~settings request =
  let _ = request in
  let page = User.page settings in

  Dream_html.respond page

let change_url_lang (url : string) ~(curr_lang : string) ~(new_lang : string) =
  let current_url = Uri.of_string url in
  let path = Uri.path current_url in
  let base_url =
    String.substr_replace_first
      (Uri.to_string current_url)
      ~pattern:path ~with_:""
  in
  let path_no_scope =
    String.substr_replace_first path ~pattern:(sprintf "/%s" curr_lang)
      ~with_:""
  in
  let new_url = sprintf "%s/%s%s" base_url new_lang path_no_scope in
  new_url

let with_settings handler request =
  (* TODO: this looks very ugly but I can't think of a better way to handle this *)
  let do_redirect = ref false in
  let redirect_url = ref None in

  let translations =
    match Dream.field request translations_field with
    | None -> failwith "handler is not under the `/:lang` scope"
    | Some t -> t
  in

  (* try to get dark-mode from query, if provided then update session value and
     use the query value, if not provided then use the session value or default
     *)
  let dark_mode =
    match Dream.query request "dark-mode" with
    | None -> (
        match Dream.session_field request dark_mode_field with
        | None -> "0"
        | Some mode -> mode)
    | Some mode ->
        ignore (Dream.set_session_field request dark_mode_field mode);
        do_redirect := true;
        redirect_url := Dream.header request "HX-Current-URL";

        mode
  in

  let settings = Page_settings.Fields.create ~translations ~dark_mode in

  (* NOTE: If language is provided as query parameter -> redirect to the same page but with 
     different language scope. Otherwise (if not provided) -> just render the page 
     with current translation settings *)
  ignore
    (match Dream.query request "language" with
    | Some query_lang ->
        let current_url =
          Option.value_exn @@ Dream.header request "HX-Current-URL"
        in
        let new_url =
          change_url_lang current_url ~curr_lang:settings.translations.lang
            ~new_lang:query_lang
        in
        do_redirect := true;
        redirect_url := Some new_url
    | None -> ());

  if !do_redirect then
    (* NOTE: In general the redirect codes are 3XX but HTMX docs say that 
         they don't process response headers if code is set to 3XX (even though
         it works) so here we set the code to 200 
          https://htmx.org/headers/hx-redirect/  *)
    Dream_html.respond ~code:200
      ~headers:[ ("HX-Redirect", Option.value_exn !redirect_url) ]
      (Dream_html.HTML.null [])
  else handler ~settings request

let lang_middleware inner_handler req =
  let lang = Dream.param req "lang" in
  let language = Localization.language_of_abbrev lang in
  let translation = Localization.translation_of_language language in
  Dream.set_field req translations_field translation;

  inner_handler req

let run ~(db : Db.t) =
  let state = State.load "state.json" in
  (* NOTE: rotate cookie secret about once per year, you can use the code bellow to generate it  *)
  (* let secret = Dream.to_base64url (Dream.random 32) in *)
  let secret = "9RV8f8QqR6foKzdX51ZMXB68C9apHx8VNkbEmJ17nWE" in
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger @@ Dream.set_secret secret
  (* TODO: consider making this indefinite ? we don't have login etc. so no
     point in refreshing cookies especially if we store some data like
     favoritted runners there etc. *)
  @@ Dream.cookie_sessions
  @@ Dream.router
       [
         (* TODO: I hate that the "/:lang" and "/en/" here are not
            Paths.index_en (i.e. Dream_html paths) *)
         Dream.scope "/:lang" [ lang_middleware ]
           [
             Dream_html.get Paths.index
               (with_settings (handle_index ~db ~state));
             Dream_html.get Paths.user (with_settings handle_user);
           ];
         Dream_html.get Paths.index (fun req -> Dream.redirect req "/en/");
         Static.routes;
       ]
