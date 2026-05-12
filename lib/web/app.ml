open Core

let language_field =
  Dream.new_field () ~name:"language" ~show_value:Localization.show_language

let handle_index request =
  let lang =
    match Dream.field request language_field with
    | None -> Localization.English
    | Some l -> l
  in
  let translations = Localization.translation_of_language lang in
  let page = Index.page translations in

  Dream_html.respond page

let handle_user request =
  (* TODO: each link that a handler returns must have the correct language
     preffix (how to join the lang prefix and the paths????) *)
  let lang =
    match Dream.field request language_field with
    | None -> Localization.English
    | Some l -> l
  in
  let translations = Localization.translation_of_language lang in
  let open Dream_html in
  let open HTML in
  let page =
    html
      [ lang "en" ]
      [
        head [] [];
        body [] [ h1 [] [ txt "Hello %s" (translations Localization.Runner) ] ];
      ]
  in

  respond page

let lang_middleware inner_handler req =
  let lang = Dream.param req "lang" in
  let language = Localization.language_of_abbrev lang in
  Dream.set_field req language_field language;

  inner_handler req

let run ~(db : Db.t) =
  let _ = db in
  (* NOTE: rotate cookie secret about once per year, you can use the code bellow to generate it  *)
  (* let secret = Dream.to_base64url (Dream.random 32) in *)
  let secret = "9RV8f8QqR6foKzdX51ZMXB68C9apHx8VNkbEmJ17nWE" in
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger @@ Dream.set_secret secret
  (* TODO: consider making this indefinite ? we don't have login etc. so no
     point in refreshing cookies especially if we store some data like
     favoritted runners there etc. *)
  @@ Dream.cookie_sessions ~lifetime:86400.0 (* lifetime of 1 day *)
  @@ Dream.router
       [
         (* TODO: I hate that the "/:lang" and "/en/" here are not
            Paths.index_en (i.e. Dream_html paths) *)
         Dream.scope "/:lang" [ lang_middleware ]
           [
             Dream_html.get Paths.index handle_index;
             Dream_html.get Paths.user handle_user;
           ];
         Dream_html.get Paths.index (fun req -> Dream.redirect req "/en/");
         Static.routes;
       ]
