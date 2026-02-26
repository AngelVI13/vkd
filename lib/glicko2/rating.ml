open Core

let test () = printf "hello world"

let%expect_test "test" =
  test ();
  [%expect {| hello world |}]
