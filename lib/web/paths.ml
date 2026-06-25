let%path index = "/"
let%path index_w_scope = "/%s/"

(* under scope: /:lang *)
let%path user = "/user"
let%path user_w_scope = "/%s/user"
let%path rating_table = "/rating_table"
let%path rating_table_w_scope = "/%s/rating_table"
let%path rating_table_w_scope_w_page = "/%s/rating_table?page=%d"
