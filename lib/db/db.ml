open Core
open Db_ops
module DB = DbOps (Turso)

type t = Turso.conn [@@deriving show { with_path = false }]

let make ~hostname ~token : Turso.conn =
  (* TODO: check if db exists and if not, create it *)
  (* TODO: if you want to make use of the baton you have to first execute the
     following req "sql": "BEGIN"  *)
  {
    hostname;
    token;
    log_name = "db_logs.txt";
    baton = None;
    immediate = true;
    statements = [];
  }

let log_db_conn t =
  Turso.log_conn t (sprintf "\n\n\t>>>NEW CONN (%s) <<<\n\n" t.hostname);
  ()

let to_int64_option (i : int option) =
  match i with None -> None | Some value -> Some (Int64.of_int value)

let to_int_option (i : Int64.t option) =
  match i with None -> None | Some value -> Some (Int64.to_int_exn value)

let create_tables (handle : Turso.conn) =
  handle.immediate <- false;
  let _ = DB.create_leagues handle in
  handle.immediate <- true;
  let _ = DB.create_events handle in
  ()

(* NOTE: this is not needed for turso connection *)
let close _ = Ok ()

let%expect_test "make" =
  printf "hello";
  [%expect {| hello |}]
