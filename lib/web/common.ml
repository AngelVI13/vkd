open! Core
open Dream_html
open HTML

let head_elems (dark_mode : string) =
  [
    meta [ http_equiv `content_type; content "text/html; charset=UTF-8" ];
    meta [ charset "UTF-8" ];
    meta [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
    link
      [
        rel "icon";
        type_ "image/png";
        (* path_attr href Static.Assets.Images.logo3_png; *)
        (* path_attr href Static.Assets.Images.logo5_png; *)
        path_attr href Static.Assets.Images.vkd_logo_png;
      ];
    title [] "O-Stats";
    (* NOTE: htmx version 2.0.7 *)
    script [ path_attr src Static.Assets.Js.Htmx.Dist.htmx_min_js ] "";
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href
          (if String.(dark_mode = "1") then Static.Assets.Css.colors_dark_css
           else Static.Assets.Css.colors_light_css);
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.common_css;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.header_css;
      ];
  ]

type ratingCourse = Course1 | Course2 | Course3 | CourseD
[@@deriving enumerate, eq]

let show_ratingCourse = function
  | Course1 -> "1"
  | Course2 -> "2"
  | Course3 -> "3"
  | CourseD -> "D"

let ratingCourse_of_string = function
  | "1" -> Course1
  | "2" -> Course2
  | "3" -> Course3
  | "D" -> CourseD
  | _ -> assert false

let ratingCourse_eq (rating_course : ratingCourse) (course_id : string) : bool =
  String.equal (show_ratingCourse rating_course) course_id
