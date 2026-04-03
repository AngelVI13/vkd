open Core

let calculate_percent (part : int) (whole : int) =
  assert (whole > 0);
  Float.(to_int (round_nearest 100.0 * (of_int part / of_int whole)))

(* time is in the form h:mm:ss or mm:ss *)
let time_of_string (t : string) : int option =
  if String.count t ~f:(fun c -> Char.(c = ':')) < 1 then None
  else
    let parts = String.split t ~on:':' in
    List.foldi (List.rev parts) ~init:0 ~f:(fun i acc part ->
        let time = Int.of_string part in
        if i = 0 then time
        else acc + (time * Float.to_int (60. ** Float.of_int i)))
    |> Some

let iso8601_of_date ?(end_of_day = false) date =
  let time_data =
    if end_of_day then Time_ns_unix.Ofday.create ~hr:23 ~min:59 ~sec:59 ()
    else Time_ns_unix.Ofday.create ~hr:0 ~min:0 ~sec:0 ()
  in
  let now = Time_ns_unix.of_date_ofday ~zone:Timezone.utc date time_data in
  Time_ns_unix.to_string_iso8601_basic ~zone:Timezone.utc now

let iso8601_to_date timestamp =
  let time = Time_ns_unix.of_string timestamp in
  (* let time = Time_ns.of_string_with_utc_offset timestamp in *)
  let date, _ = Time_ns_unix.to_date_ofday ~zone:Timezone.utc time in
  date

let format_time_as_date (time : Time_ns_unix.t) =
  Time_ns_unix.format ~zone:Timezone.utc time "%Y-%m-%d"

let%expect_test "time_of_string" =
  let time = time_of_string "1:02:03" in
  printf "%d" (Option.value_exn time);
  [%expect {| 3723 |}];

  let time = time_of_string "1:00:03" in
  printf "%d" (Option.value_exn time);
  [%expect {| 3603 |}];

  let time = time_of_string "5:27:13" in
  printf "%d" (Option.value_exn time);
  [%expect {| 19633 |}];

  let time = time_of_string "02:03" in
  printf "%d" (Option.value_exn time);
  [%expect {| 123 |}];

  let time = time_of_string "02" in
  printf "%b" (Option.is_none time);
  [%expect {| true |}]
