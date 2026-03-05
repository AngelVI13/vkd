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
