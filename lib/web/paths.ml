let%path index = "/"
let%path index_w_scope = "/%s/"

(* under scope: /:lang *)
let%path user = "/user"
let%path user_w_scope = "/%s/user?runner_id=%d"
let%path user_tab = "/user_tab"
let%path user_tab_w_scope = "/%s/user_tab?runner_id=%d?tab=%s"
let%path rating_table = "/rating_table"
let%path rating_table_w_scope = "/%s/rating_table"
let%path rating_table_w_scope_w_page = "/%s/rating_table?page=%d"
