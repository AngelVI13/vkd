open Core

module CustomDbOps (T : Sqlgg_traits.M) = struct
  module IO = Sqlgg_io.Blocking

  let all_latest_relevant_ratings db callback ~participants =
    let invoke_callback stmt =
      callback ~id:(T.get_column_Int stmt 0)
        ~league_id:(T.get_column_Int stmt 1) ~event_nr:(T.get_column_Int stmt 2)
        ~event_date:(T.get_column_Text stmt 3)
        ~course_id:(T.get_column_Text stmt 4)
        ~runner_id:(T.get_column_Int stmt 5)
        ~rating:(T.get_column_Float stmt 6)
        ~rating_diff:(T.get_column_Float stmt 7)
        ~rd:(T.get_column_Float stmt 8)
        ~vol:(T.get_column_Float stmt 9)
        ~runner_name:(T.get_column_Text stmt 10)
        ~runner_club:(T.get_column_Text stmt 11)
        ~runner_gender:(T.get_column_Text stmt 12)
    in
    let participants =
      List.map participants ~f:(fun p -> sprintf "(%d)" p)
      |> String.concat ~sep:","
    in
    T.select db
      (sprintf
         "WITH participants(runner_id) AS (\n\
          VALUES %s\n\
          )\n\
          SELECT r.*, rn.name AS runner_name, rn.club AS runner_club, \
          rn.gender AS runner_gender,\n\
          CASE WHEN p.runner_id IS NOT NULL THEN 1 ELSE 0 END AS participated\n\
          FROM ratings r\n\
          JOIN runners rn ON r.runner_id = rn.id\n\
          INNER JOIN (\n\
          SELECT runner_id, MAX(event_date) AS max_date\n\
          FROM ratings\n\
          GROUP BY runner_id\n\
          ) latest ON r.runner_id = latest.runner_id AND r.event_date = \
          latest.max_date\n\
          LEFT JOIN participants p ON r.runner_id = p.runner_id\n\
          WHERE\n\
          p.runner_id IS NOT NULL\n\
          OR r.rd < 350;\n"
         participants)
      T.no_params invoke_callback
end
(* module DbOps *)
