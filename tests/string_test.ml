open OUnit2

let test_repeat = "repeat string" >:: (fun _ -> assert_equal (MLPystring.repeat "123" 2) "123123")

let test_char_of_string = "cast char to string" >:: (fun _ -> assert_equal (MLPystring.char_of_string '1') "1")

let tests = "all_tests" >::: [
  test_repeat;
  test_char_of_string;
  ]
