open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = Split.t list [@@deriving yojson, show]

let of_string ~(start : int) s : t =
  let timestamps =
    String.split s ~on:'-'
    |> List.map ~f:(fun time ->
           let time = String.strip time in
           if String.(time = "") then None else Some (Int.of_string time))
  in
  let splits =
    List.mapi timestamps ~f:(fun idx time ->
        let prev_time =
          if idx = 0 then Some start else List.nth_exn timestamps (idx - 1)
        in
        let control_time =
          Option.bind prev_time ~f:(fun prev ->
              Option.bind time ~f:(fun t ->
                  let time = t - prev in
                  if time > 0 then Some time else None))
        in
        let overall_time, timestamp =
          match time with
          | None -> (None, None)
          | Some t -> (Some (t - start), Some t)
        in

        Split.make ~time:control_time ~overall_time ~timestamp)
  in
  splits

(* if any split is missing a control time for any reason then this runner should be DSQ *)
let has_dsq (t : t) =
  let bad_splits = List.filter t ~f:(fun split -> Option.is_none split.time) in
  List.length bad_splits > 0
