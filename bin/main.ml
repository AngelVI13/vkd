open Core

let init_db ~debug =
  let db_hostname = Sys.getenv_exn "TURSO_DB_HOSTNAME" in
  let db_token = Sys.getenv_exn "TURSO_DB_TOKEN" in
  let db = Db.make ~debug ~hostname:db_hostname ~token:db_token () in
  Db.create_tables db;
  db

let command_test () =
  Command.basic ~summary:"Test things"
    (let%map_open.Command debug =
       flag "-v" (optional_with_default false bool) ~doc:"Debug logs flag\n"
     in
     fun () ->
       let db = init_db ~debug in
       Db.test db;
       Or_error.ok_exn (Db.close db))

let command_app () =
  Command.basic ~summary:"Run the web app."
    (let%map_open.Command debug =
       flag "-v" (optional_with_default false bool) ~doc:"Verbose flag\n"
     in
     fun () ->
       let db = init_db ~debug in
       Fun.protect
         ~finally:(fun () ->
           printf "Closing the db\n";
           ignore @@ Db.close db)
         (fun () -> Web.App.run ~db))

let command () =
  Command.group
    ~summary:
      "App to download, process & display data from Vilniaus ketvirtadieniai"
    [ ("test", command_test ()); ("app", command_app ()) ]

let () =
  Dotenv.export () |> ignore;
  Command_unix.run (command ())
