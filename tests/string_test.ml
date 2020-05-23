open OUnit2

let test_repeat = "repeat string" >:: (fun _ -> assert_equal (MLPystring.repeat "123" 2) "123123")

let tests = "all_tests" >::: [test_repeat;]
