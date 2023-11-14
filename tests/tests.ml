(*
  We will put the tests for functions here
*)

(* Will comment back in soon *)
(* open Core;;
open OUnit2;; *)

open Core
open OUnit2
open Math


let v2a = Vec2.of_list [1.; 2.]
let v2b = Vec2.of_list [-1.; 0.5]

let v2c = Vec2.of_list [233.; -0.7]
let v3a = Vec3.of_list [1.; 2.; 2.0]
let v3b = Vec3.of_list [-1.; 0.5; 0.2]

let v3c = Vec3.of_list [-0.23; 1.34; 43.]

let float_equal f1 f2 = (Float.compare (Float.abs (f1 -. f2)) 0.00000001) = -1
let float_list_equal lf1 lf2 = List.for_all2_exn lf1 lf2 ~f:float_equal
let math_tests_ _ =  
  assert_equal 1. @@ Vec2.nth v2a 0;
  assert_equal [1.; 2.] @@ Vec2.to_list v2a;
  assert_equal [0.; 2.5] @@ Vec2.to_list (Vec2.(+) v2a v2b);
  assert_equal [0.; 2.5; 2.2] @@ Vec3.to_list (Vec3.(+) v3a v3b);
  assert_equal [2.; 1.5] @@ Vec2.to_list (Vec2.(-) v2a v2b);
  assert_equal [-2.; -1.5; -1.8] @@ Vec3.to_list (Vec3.(-) v3b v3a);
  assert_equal [2.; 1.5] @@ Vec2.to_list (Vec2.(-) v2a v2b);
  assert_equal [-2.; -1.5; -1.8] @@ Vec3.to_list (Vec3.(-) v3b v3a);
  assert_equal [1.5; 3.] @@ Vec2.to_list (Vec2.( * ) 1.5 v2a);
  assert_equal [-2.; 1.; 0.4] @@ Vec3.to_list (Vec3.( * ) 2. v3b);
  assert_equal 5. @@ Vec2.sqr_length v2a;
  assert_equal 9. @@ Vec3.sqr_length v3a;
  assert_equal (sqrt 5.) @@ Vec2.length v2a;
  assert_equal 3. @@ Vec3.length v3a;
  assert_equal [1. /. (sqrt 5.); 2. /. (sqrt 5.)] @@ Vec2.to_list (Vec2.unit v2a);
  assert_equal [1. /. 3. ; 2. /. 3. ; 2. /. 3.] @@ Vec3.to_list (Vec3.unit v3a);
  assert_equal 0. @@ Vec2.dot v2a v2b;
  assert_equal 0.4 @@ Vec3.dot v3a v3b;
  assert_equal true @@ float_equal (rad 30.) 0.5235987755982988;
  assert_equal [-0.6; -2.2; 2.5] @@ Vec3.to_list (cross v3a v3b);
  assert_equal true @@ float_list_equal [0.011219512195121953; -0.06536585365853659] (Vec2.to_list @@ sProj v3c ~z_c:1.);
  assert_equal true @@ float_list_equal [0.029178072983855743; -8.765944673261381e-05; 3.3995742255444417] (Vec3.to_list @@ invSProj v2c ~z_c:2.4);
  assert_equal true @@ float_list_equal [18.353099910975896; -0.15040829928574848] (Vec2.to_list (mapMoebius (Vec2.of_list [233.; -0.7]) ~alpha:0.1 ~beta:0.2 ~center:(Vec3.of_list [0.;0.;1.])))
  
let math_tests = "Specifications" >: test_list [
  "math_tests" >:: math_tests_
]





let series = "Project Tests" >::: [ math_tests ]

let () = run_test_tt_main series

(* Vec2.to_list (mapMoebius (Vec2.of_list [233.; -0.7]) ~alpha:0.1 ~beta:0.2 ~center:(Vec3.of_list [0.;0.;1.]));;*)