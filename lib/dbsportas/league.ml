open Core

(* https://dbsportas.lt/lt/mvarz/244 *)
let fetch_league () =
  let url = "https://dbsportas.lt/lt/mvarz/244" in
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  Out_channel.write_all "/home/angel/Documents/ocaml/vkd/league.html" ~data:out;
  (* printf "Hello world" *)
  ()

module LeagueEvent = struct
  type t = {
    nr : int;
    date : string;
    location : string;
    results_url : string option;
  }
  [@@deriving show { with_path = false }]

  let of_td_list ~results_url td_list =
    assert (List.length td_list = 3);
    {
      nr = Int.of_string @@ List.nth_exn td_list 0;
      date = List.nth_exn td_list 1;
      location = List.nth_exn td_list 2;
      results_url;
    }
end

module League = struct
  type t = LeagueEvent.t list [@@deriving show { with_path = false }]
end

let parse_league_page () =
  let open Soup in
  let filename = "/home/angel/Documents/ocaml/vkd/league.html" in
  let soup = read_file filename |> parse in
  let rows = soup $ ".w3-table" $$ "tr" |> to_list in
  let tds =
    rows
    |> List.fold ~init:[] ~f:(fun acc tr ->
           let results_url =
             tr $? ".w3-text-green"
             |> Option.bind ~f:(fun a -> Some (R.attribute "href" a))
           in
           let tds = tr $$ "td" |> to_list |> List.map ~f:R.leaf_text in
           match tds with
           | [] -> acc
           | _ -> LeagueEvent.of_td_list ~results_url tds :: acc)
  in
  List.iter tds ~f:(fun ev -> printf "%s\n" (LeagueEvent.show ev));
  printf ""

let%expect_test "parse_league_page" =
  parse_league_page ();
  [%expect {||}]
