open OUnit2

let test_repeat = "repeat string" >::
 (fun _ -> assert_equal (Mlpystring.repeat "123" 2) "123123")

let test_char_of_string = "cast char to string" >::
 (fun _ -> assert_equal (Mlpystring.char_of_string '1') "1")

let test_at = "get char" >::
 (fun _ -> assert_equal (Mlpystring.at "012345", 0) '0')

let test_center = "centerlize string" >::
  (fun _ -> assert_equal (Mlpystring.center "aaa", 5) " aaa ")

let tests = "all_tests" >::: [
  test_repeat;
  test_char_of_string;
  test_at;
  test_center;
]
