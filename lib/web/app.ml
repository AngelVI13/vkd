open Core

let handle_index request =
  let _ = request in
  let page = Index.page () in

  Dream_html.respond page

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
  @@ Dream.router [ Dream_html.get Paths.index handle_index; Static.routes ]
