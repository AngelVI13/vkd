open Core

let command_test () =
  Command.basic ~summary:"Test things"
    (let%map_open.Command debug =
       flag "-v" (optional_with_default false bool) ~doc:"Debug logs flag\n"
     in
     fun () ->
       let db_hostname = Sys.getenv_exn "TURSO_DB_HOSTNAME" in
       let db_token = Sys.getenv_exn "TURSO_DB_TOKEN" in
       let db = Db.make ~debug ~hostname:db_hostname ~token:db_token () in
       Db.create_tables db;
       Db.test_add_leagues db Dbsportas.League.leagues;
       Or_error.ok_exn (Db.close db))

let command () =
  Command.group
    ~summary:
      "App to download, process & display data from Vilniaus ketvirtadieniai"
    [ ("test", command_test ()) ]

let () =
  Dotenv.export () |> ignore;
  Command_unix.run (command ())
