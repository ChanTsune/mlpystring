open OUnit2

let test_repeat = "repeat string" >::
 (fun _ -> assert_equal (Mlpystring.repeat "123" 2) "123123")

let test_char_of_string = "cast char to string" >::
 (fun _ -> assert_equal (Mlpystring.char_of_string '1') "1")

let test_at = "get char" >::
 (fun _ -> assert_equal (Mlpystring.at "012345" 0) '0')

let test_center = "centerlize string" >::
  (fun _ -> assert_equal (Mlpystring.center "aaa" 5) " aaa ")

(* TODO: Test *)

let test_capitalize = "" >:: (fun _ -> assert_equal (Mlpystring.capitalize "abc") "Abc")
let test_count = "" >:: (fun _ -> assert_equal (Mlpystring.count "abc" "a") 1)
let test_endswith = "" >:: (fun _ -> assert_equal (Mlpystring.endswith "abc" "c") true)
let test_find = "" >:: (fun _ -> assert_equal (Mlpystring.find "abc" "a") 0)
let test_get = "" >:: (fun _ -> assert_equal (Mlpystring.get "abc" 0) 'a')
let test_isalnum = "" >:: (fun _ -> assert_equal (Mlpystring.isalnum "abc") true)
let test_isalpha = "" >:: (fun _ -> assert_equal (Mlpystring.isalpha "abc") true)
let test_isascii = "" >:: (fun _ -> assert_equal (Mlpystring.isascii "abc") true)
let test_isdecimal = "" >:: (fun _ -> assert_equal (Mlpystring.isdecimal "012") true)
let test_isdigit = "" >:: (fun _ -> assert_equal (Mlpystring.isdigit "012") true)
let test_islower = "" >:: (fun _ -> assert_equal (Mlpystring.islower "abc") true)
let test_isnumeric = "" >:: (fun _ -> assert_equal (Mlpystring.isnumeric "012") true)
let test_isprintable = "" >:: (fun _ -> assert_equal (Mlpystring.isprintable "21") true)
let test_isspace = "" >:: (fun _ -> assert_equal (Mlpystring.isspace " ") true)
let test_isupper = "" >:: (fun _ -> assert_equal (Mlpystring.isupper "A") true)
let test_ljust = "" >:: (fun _ -> assert_equal (Mlpystring.ljust "12" 3) " 12")
let test_lower = "" >:: (fun _ -> assert_equal (Mlpystring.lower "ABC") "abc")
let test_lstrip = "" >:: (fun _ -> assert_equal (Mlpystring.lstrip " a") "a")
let test_join = "" >:: (fun _ -> assert_equal (Mlpystring.join "a" ["b";"c"]) "bac")
let test_rjust = "" >:: (fun _ -> assert_equal (Mlpystring.rjust "12" 3) " 12")
let test_rstrip = "" >:: (fun _ -> assert_equal (Mlpystring.rstrip "12 ") "12")
let test_replace = "" >:: (fun _ -> assert_equal (Mlpystring.replace "123" "2" "4") "143")
let test_split = "" >:: (fun _ -> assert_equal (Mlpystring.split "1,2" ",") ["1"; "2"])
let test_strip = "" >:: (fun _ -> assert_equal (Mlpystring.strip " 12 ") "12")
let test_partition = "" >:: (fun _ -> assert_equal (Mlpystring.partition "1,2" ",") ["1"; "2"])
let test_splitlines = "" >:: (fun _ -> assert_equal (Mlpystring.splitlines "line") ["line"])
let test_index = "" >:: (fun _ -> assert_equal (Mlpystring.index "0" "0") 0)

let tests = "all_tests" >::: [
  test_repeat;
  test_char_of_string;
  test_at;
  test_center;
  test_capitalize;
  test_count;
  test_endswith;
  test_find;
  test_get;
  test_isalnum;
  test_isalpha;
  test_isascii;
  test_isdecimal;
  test_isdigit;
  test_islower;
  test_isnumeric;
  test_isprintable;
  test_isspace;
  test_isupper;
  test_ljust;
  test_lower;
  test_lstrip;
  test_join;
  test_rjust;
  test_rstrip;
  test_replace;
  test_split;
  test_strip;
  test_partition;
  test_splitlines;
  test_index;
]
