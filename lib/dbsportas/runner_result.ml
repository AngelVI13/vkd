open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type resultStatus = Finished | Dsq [@@deriving yojson, eq]

type t = {
  number : int;
  name : string;
  club : string;
  start : int;
  status : resultStatus;
  time : int;
  splits : Splits.t;
}
[@@deriving fields ~fields ~iterators:create, yojson]

let of_resp (runner : Response.RunnerResp.t) =
  let splits = Splits.of_string ~start:runner.start runner.splits in
  let finish = List.last_exn splits in
  (* `value_exn` here should be safe because everyone should have a finish time (i think?) *)
  let time = Option.value_exn finish.overall_time in
  (* this is different than the `flag` value, it is determined by the actual data in the splits *)
  let has_dsq = Splits.has_dsq splits in

  Fields.create ~number:runner.number ~name:runner.name ~club:runner.club
    ~start:runner.start ~time
    ~status:(if runner.flag = 0 && not has_dsq then Finished else Dsq)
    ~splits

let update_position_for_split ~field r split_idx position =
  let splits =
    List.mapi r.splits ~f:(fun i split ->
        if i = split_idx then Field.fset field split (Some position) else split)
  in
  { r with splits }
