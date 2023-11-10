open OUnit2
open Submission

[@@@ocaml.warning "-32"]

(* This file contains a few tests but not necessarily complete coverage.  You are
   encouraged to think of more tests for the corner cases.
   We will cover the details of the test syntax later, but with simple copy/paste it should
   not be hard to add new tests of your own without knowing the details.
   1) Write a new let which performs the test, e.g. let test_fibonacci_2 _ = ...
   2) Add that let-named entity to one of the test suite lists such as section1_tests
      by adding e.g.
       "Fibonacci 2"       >:: test_fibonacci_2;
   Thats it!

   Recall that you need to type "dune test" to your shell to run the test suite.
*)

let test_summate _ =
  assert_equal (summate 3) 6;
  assert_equal (summate 10) 55 (* note a semicolon is illegal here - OCaml oddity *)
  
let test_lcm _ =
  assert_equal (lcm 3 9) 9;
  assert_equal (lcm 9 12) 36;
  (* My test (1) added below *)
  assert_equal (lcm 5 8) 40

let test_fibonacci _ =
  assert_equal (fibonacci 0) 0;
  assert_equal (fibonacci 10) 55;
  (* My tests (2) added below *)
  assert_equal (fibonacci 1) 1;
  assert_equal (fibonacci 2) 1
  


let part1_section1_tests =
  "Part 1 Section 1" >::: [
    "Summate" >:: test_summate;
    "LCM" >:: test_lcm;
    "Fibonacci" >:: test_fibonacci;
  ]

let test_iota1 _ = assert_equal (iota1 5) [ 5; 4; 3; 2; 1 ]
let test_iota2 _ = assert_equal (iota2 6) [ 1; 2; 3; 4; 5; 6 ]

let test_factors _ =
  assert_equal (factors 10) [ 1; 2; 5; 10 ];
  assert_equal (factors 12) [ 1; 2; 3; 4; 6; 12 ];
  (* My tests (1) added below *)
  assert_equal (factors 19) [ 1; 19 ]


let test_reverse _ = assert_equal (reverse [1;2;3;4;5]) [5;4;3;2;1]

let test_split_list _ = assert_equal (split_list [1;2;3;4;5] 3) ([1;2;3], [4;5]);
  (* My tests (5) added below *)
  assert_equal (split_list [1;2;3;4;5] 0) ([], [1;2;3;4;5]);
  assert_equal (split_list [1;2;3;4;5] 1) ([1], [2;3;4;5]);
  assert_equal (split_list [1;2;3;4;5] 2) ([1;2], [3;4;5]);
  assert_equal (split_list [1;2;3;4;5] 4) ([1;2;3;4], [5]);
  assert_equal (split_list [1;2;3;4;5] 5) ([1;2;3;4;5], [])

let test_rotate_list _ = assert_equal (rotate_list [1;2;3;4;5] 3) [3;4;5;1;2];
  (* My tests (6) added below *)
  assert_equal (rotate_list [1;2;3;4;5] 1) [5;1;2;3;4];
  assert_equal (rotate_list [1;2;3;4;5] 2) [4;5;1;2;3];
  assert_equal (rotate_list [1;2;3;4;5] 4) [2;3;4;5;1];
  assert_equal (rotate_list [1;2;3;4;5] 5) [1;2;3;4;5];
  assert_equal (rotate_list [1;2;3;4;5] 10) [1;2;3;4;5];
  assert_equal (rotate_list [1;2;3;4;5;6] 1) [6;1;2;3;4;5]
  

let part1_section2_tests =
  "Part 1 Section 2" >::: [
      "Iota1" >:: test_iota1;
      "Iota2" >:: test_iota2;
      "Factors" >:: test_factors;
      "Reverse" >:: test_reverse;
      "Split List" >:: test_split_list;
      "Rotate List" >:: test_rotate_list;
  ]

let test_remove_max _ =
  assert_equal (remove_max [ "hi"; "remove me!"; "don't remove me" ])
  @@ Ok ("remove me!", [ "hi"; "don't remove me" ])

let test_max_sort _ =
  assert_equal
    (max_sort [ "bus"; "cat"; "apple"; "green"; "email" ])
    [ "apple"; "bus"; "cat"; "email"; "green" ]

let test_merge_sort _ = assert_equal (merge_sort [5;1;3;2;4]) [1;2;3;4;5]

let part1_section3_tests =
  "Part 1 Section 3" >::: [
    "Remove Max" >:: test_remove_max;
    "Max Sort" >:: test_max_sort;
    "Merge Sort" >:: test_merge_sort;
  ]
      

(* Part II Section 1: we leave this section to you *)

let test_iota1' _ = assert_equal (iota1' 5) [ 5; 4; 3; 2; 1 ]
let test_iota2' _ = assert_equal (iota2' 6) [ 1; 2; 3; 4; 5; 6 ]

let test_factors' _ =
  assert_equal (factors' 10) [ 1; 2; 5; 10 ];
  assert_equal (factors' 12) [ 1; 2; 3; 4; 6; 12 ];
  (* My tests (1) added below *)
  assert_equal (factors' 19) [ 1; 19 ]

let test_split_list' _ = assert_equal (split_list' [1;2;3;4;5] 3) ([1;2;3], [4;5]);
  (* My tests (5) added below *)
  assert_equal (split_list' [1;2;3;4;5] 0) ([], [1;2;3;4;5]);
  assert_equal (split_list' [1;2;3;4;5] 1) ([1], [2;3;4;5]);
  assert_equal (split_list' [1;2;3;4;5] 2) ([1;2], [3;4;5]);
  assert_equal (split_list' [1;2;3;4;5] 4) ([1;2;3;4], [5]);
  assert_equal (split_list' [1;2;3;4;5] 5) ([1;2;3;4;5], [])

(* I hope that the double colons below in >:: are okay *)
let part2_section1_tests = "Part 2 Section 1" >: test_list [
  "Iota 1" >:: test_iota1';
  "Iota 2" >:: test_iota2';
  "Factors" >:: test_factors';
  "Split List" >:: test_split_list';
]

(* Part II Section 2: primes *)
let test_is_prime _ =
  assert_equal (is_prime 2) true;
  assert_equal (is_prime 3) true;
  (* My tests (10) added below *)
  assert_equal (is_prime 4) false;
  assert_equal (is_prime 5) true;
  assert_equal (is_prime 6) false;
  assert_equal (is_prime 7) true;
  assert_equal (is_prime 8) false;
  assert_equal (is_prime 9) false;
  assert_equal (is_prime 10) false;
  assert_equal (is_prime 11) true;
  assert_equal (is_prime 12) false;
  assert_equal (is_prime 13) true

let test_is_prime_large _ = assert_equal (is_prime (Core.Int.pow 2 31 - 1)) true;
  (* My tests (2) added below *)
  (* 17 and 19 are from the sequence of Mersenne exponents
     https://oeis.org/A000043 *)
  assert_equal (is_prime (Core.Int.pow 2 17 - 1)) true;
  assert_equal (is_prime (Core.Int.pow 2 19 - 1)) true

let test_prime_factor_with_greatest_multiplicity _ =
  assert_equal (prime_factor_with_greatest_multiplicity 4) 2;
  assert_equal (prime_factor_with_greatest_multiplicity 120) 2;
  assert_equal (prime_factor_with_greatest_multiplicity 5) 5;
  assert_equal (prime_factor_with_greatest_multiplicity 252) 3

let part2_section2_tests =
  "Part 2 Section 2" >::: [
    "Is Prime" >:: test_is_prime;
    "Is Prime Large" >:: test_is_prime_large;
    "Prime Factor with Greatest Multiplicity" >:: test_prime_factor_with_greatest_multiplicity;
  ]

(* Part II Section 3 *)
let tower_board_example_2x2 = [ [ 1; 2 ]; 
                                [ 2; 1 ] ]
let tower_board_ill_formed_example_2x2 = [ [ 1; 2 ]; 
                                           [ 2; 2 ] ]
let tower_board_not_square_example = [ [ 1; 2; 3 ]; 
                                       [ 2; 1 ] ]
let tower_board_example_3x3 = [ [ 1; 2; 3 ]; 
                                [ 3; 1; 2 ]; 
                                [ 2; 3; 1 ] ]
let tower_board_example_transposed_3x3 = [ [ 1; 3; 2 ];
                                           [ 2; 1; 3 ];
                                           [ 3; 2; 1 ] ]
let tower_board_example_reflected_3x3 = [ [ 3; 2; 1 ];
                                          [ 2; 1; 3 ];
                                          [ 1; 3; 2 ] ]
let tower_board_ill_formed_3x3 = [ [ 1; 2; 3];
                                   [ 3; 2; 1];
                                   [ 2; 3; 1] ]

let test_square_size _ = 
  assert_equal (square_size tower_board_example_2x2) (Ok 2);
  assert_equal (square_size tower_board_example_3x3) (Ok 3);
  assert_equal (square_size tower_board_not_square_example) (Error "not square")
  
let test_elements_span_range _ = 
  assert_equal (elements_span_range [3;2;1]) true;
  assert_equal (elements_span_range [3;2;2]) false;
  assert_equal (elements_span_range [1;7;3;4;5;6;7]) false
 
let test_well_formed_grid _ =
  assert_equal (well_formed_grid tower_board_example_2x2) true;
  assert_equal (well_formed_grid tower_board_ill_formed_example_2x2) false;
  assert_equal (well_formed_grid tower_board_ill_formed_3x3) false
  
let test_local_max_count _ =
  assert_equal (local_max_count [1;2;3]) 3;
  assert_equal (local_max_count [3;2;1]) 1;
  assert_equal (local_max_count [1;3;2;5;4]) 3
 
let test_verify_left_clues _ =
  assert_equal (verify_left_clues tower_board_example_2x2 [ 2; 1 ]) true;
  assert_equal (verify_left_clues tower_board_example_3x3 [ 3; 1; 2]) true;
  assert_equal (verify_left_clues tower_board_example_3x3 [ 2; 1; 2]) false

let test_transpose _ =
  assert_equal (transpose tower_board_example_2x2) tower_board_example_2x2;
  assert_equal (transpose tower_board_example_3x3) tower_board_example_transposed_3x3
 
let test_reflect_vertical_axis _ =
  assert_equal (reflect_vertical_axis tower_board_example_2x2)
    [[2; 1]; 
     [1; 2]];
     
  assert_equal (reflect_vertical_axis tower_board_example_3x3) tower_board_example_reflected_3x3

(* A useful invariant: four rotations is a no-op *)
let test_rotate_ccw _ =
  assert_equal
    (tower_board_example_2x2 |> rotate_ccw |> rotate_ccw |> rotate_ccw |> rotate_ccw)
    tower_board_example_2x2;
  assert_equal (rotate_ccw tower_board_example_3x3) [ [ 3; 2; 1 ]; [ 2; 1; 3 ]; [ 1; 3; 2 ] ]

let valid_counts_example_2x2 = [ [ 2; 1 ]; [ 1; 2 ]; [ 2; 1 ]; [ 1; 2 ] ]

(* Here is the "picture" of these counts on the 2x2 example above:

    2  1  
2   1  2  1 
1   2  1  2
    1  2

    - read succesive lists of counts by rotating the whole thing ccw 90.
*)   
let valid_counts_example_3x3 = [ [ 3; 1; 2 ]; [ 1; 2; 2 ]; [ 2; 2; 1 ]; [ 2; 1; 3 ] ]
let invalid_counts_example_2x2 = [ [ 1; 2 ]; [ 2; 1 ]; [ 1; 2 ]; [ 2; 1 ] ]

let test_verify_towers_solution _ =
  assert_equal
    (verify_towers_solution tower_board_example_2x2 valid_counts_example_2x2)
    true;
  assert_equal
    (verify_towers_solution tower_board_example_3x3 valid_counts_example_3x3)
    true;
  assert_equal
    (verify_towers_solution tower_board_example_2x2 invalid_counts_example_2x2)
    false

let part2_section3_tests =
  "Part 2 Section 3" >::: [
    "Square size" >:: test_square_size;
    "Elements_span_range" >:: test_elements_span_range;
    "Well_formed_grid" >:: test_well_formed_grid;
    "Local max count" >:: test_local_max_count;
    "Verify left clues" >:: test_verify_left_clues;
    "Rotate" >:: test_rotate_ccw;
    "Verify towers solution" >:: test_verify_towers_solution;
  ]

let series =
  "Assignment1 Tests" >::: [
    part1_section1_tests;
    part1_section2_tests;
    part1_section3_tests;
    (* Keep these part 2 tests commented out while working on and submitting part 1. 
      Uncomment part 2 tests when working on and submitting part 2. *)
    part2_section1_tests;
    part2_section2_tests;
    part2_section3_tests;
  ]

(* The following line runs all the tests put together into `series` above *)

let () = run_test_tt_main series