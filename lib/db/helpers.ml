open Core

let to_int64 (i : int) = Int64.of_int i
let of_int64 (i : Int64.t) = Int64.to_int_exn i
let of_int64_opt (i : Int64.t option) = Option.map ~f:of_int64 i

let to_int64_option (i : int option) =
  match i with None -> None | Some value -> Some (to_int64 value)

let to_int_option (i : Int64.t option) =
  match i with None -> None | Some value -> Some (of_int64 value)
