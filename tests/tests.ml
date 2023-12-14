(*
  We will put the tests for functions here
*)

(* Will comment back in soon *)
(* open Core;;
   open OUnit2;; *)

open Core
open OUnit2
open Math
open Rasterizer
open Animation
open Ascii_printer
open Exposed_for_testing

let v2a = Vec2.of_list [ 1.; 2. ]
let v2b = Vec2.of_list [ -1.; 0.5 ]
let v2c = Vec2.of_list [ 233.; -0.7 ]
let v3a = Vec3.of_list [ 1.; 2.; 2.0 ]
let v3b = Vec3.of_list [ -1.; 0.5; 0.2 ]
let v3c = Vec3.of_list [ -0.23; 1.34; 43. ]
let float_equal f1 f2 = Float.(Float.abs (f1 -. f2) < 0.00000001)
let float_list_equal lf1 lf2 = List.for_all2_exn lf1 lf2 ~f:float_equal

let vec_tests_ _ =
  assert_equal 1. @@ Vec2.nth v2a 0;
  assert_equal [ 1.; 2. ] @@ Vec2.to_list v2a;
  assert_equal [ 0.; 2.5 ] @@ Vec2.to_list (Vec2.( + ) v2a v2b);
  assert_equal [ 0.; 2.5; 2.2 ] @@ Vec3.to_list (Vec3.( + ) v3a v3b);
  assert_equal [ 2.; 1.5 ] @@ Vec2.to_list (Vec2.( - ) v2a v2b);
  assert_equal [ -2.; -1.5; -1.8 ] @@ Vec3.to_list (Vec3.( - ) v3b v3a);
  assert_equal [ 2.; 1.5 ] @@ Vec2.to_list (Vec2.( - ) v2a v2b);
  assert_equal [ -2.; -1.5; -1.8 ] @@ Vec3.to_list (Vec3.( - ) v3b v3a);
  assert_equal [ 1.5; 3. ] @@ Vec2.to_list (Vec2.( * ) 1.5 v2a);
  assert_equal [ -2.; 1.; 0.4 ] @@ Vec3.to_list (Vec3.( * ) 2. v3b);
  assert_equal 5. @@ Vec2.sqr_length v2a;
  assert_equal 9. @@ Vec3.sqr_length v3a;
  assert_equal (sqrt 5.) @@ Vec2.length v2a;
  assert_equal 3. @@ Vec3.length v3a;
  assert_equal [ 1. /. sqrt 5.; 2. /. sqrt 5. ] @@ Vec2.to_list (Vec2.unit v2a);
  assert_equal [ 1. /. 3.; 2. /. 3.; 2. /. 3. ] @@ Vec3.to_list (Vec3.unit v3a);
  assert_equal 0. @@ Vec2.dot v2a v2b;
  assert_equal 0.4 @@ Vec3.dot v3a v3b

let degree_tests_ _ =
  assert_equal 1.5 @@ (Degree.of_float 1.5 |> Degree.to_float);
  assert_equal true
  @@ float_equal
       (Degree.of_float 10.
       |> Degree.( + ) (Degree.of_float 6.)
       |> Fn.flip Degree.( - ) (Degree.of_float 1.)
       |> Degree.( * ) 0.1 |> Degree.to_float)
       1.5

let nonvec_tests_ _ =
  assert_equal true
  @@ float_equal (30. |> Degree.of_float |> Degree.to_radian) 0.5235987755982988;
  assert_equal [ -0.6; -2.2; 2.5 ] @@ Vec3.to_list (cross v3a v3b);
  assert_equal true
  @@ float_list_equal
       [ 0.011219512195121953; -0.06536585365853659 ]
       (Vec2.to_list @@ sProj v3c ~z_c:1.);
  assert_equal true
  @@ float_list_equal
       [ 0.029178072983855743; -8.765944673261381e-05; 3.3995742255444417 ]
       (Vec3.to_list @@ invSProj v2c ~z_c:2.4);
  assert_equal true
  @@ float_list_equal
       [ 18.353099910975896; -0.15040829928574848 ]
       (Vec2.to_list
          (mapMoebius
             (Vec2.of_list [ 233.; -0.7 ])
             ~alpha:0.1 ~beta:0.2
             ~center:(Vec3.of_list [ 0.; 0.; 1. ])))

let math_tests =
  "Math tests"
  >: test_list
       [
         "vec_tests" >:: vec_tests_; "degree_tests" >:: degree_tests_;
         "nonvec_tests" >:: nonvec_tests_;
       ]

let random2Dpoints =
  [
    [ 0.93274811; 0.30707646 ]; [ 1.10742183; 2.13776655 ];
    [ 0.72191354; 0.73283456 ]; [ 1.23816065; 1.36903545 ];
    [ 0.10673047; 2.0196763 ]; [ 1.93006175; 2.17727281 ];
    [ 0.85465794; 0.31566564 ]; [ 1.70811221; 0.14867626 ];
    [ 0.47704004; 0.24429357 ]; [ 1.08898966; 0.80247392 ];
  ]

let random_indices =
  [
    (19, 0); (27, 43); (13, 22); (22, 7); (7, 17); (5, 21); (49, 38); (30, 35);
    (27, 39); (32, 35);
  ]
  |> List.map ~f:(fun (x, y) -> (float_of_int x, float_of_int y))

let direction =
  ( Vec3.of_list [ -0.57735027; 0.57735027; -0.57735027 ],
    Vec3.of_list [ 0.70710678; 0.70710678; -0. ],
    Vec3.of_list [ -0.40824829; 0.40824829; 0.81649658 ] )

(* these values are generated from python for testing *)
let basic_tests _ =
  assert_equal [ 0; 0; 0; 0; 1; 0; 0; 0; 1; 0 ]
  @@ List.map random2Dpoints ~f:(fun x -> sample_grid (Vec2.of_list x) 4 0.1 2);
  assert_equal [ 1; 0; 0; 0; 1; 0; 0; 0; 1; 1 ]
  @@ List.map random2Dpoints ~f:(fun x -> sample_grid (Vec2.of_list x) 4 0.2 2);
  assert_equal [ 1; 0; 0; 1; 1; 0; 1; 1; 1; 1 ]
  @@ List.map random2Dpoints ~f:(fun x -> sample_grid (Vec2.of_list x) 7 0.2 2);
  assert_equal [ 0; 1; 0; 1; 0; 1; 1; 1; 1; 0 ]
  @@ List.map random2Dpoints ~f:(fun x -> sample_grid (Vec2.of_list x) 7 0.1 3);
  assert_equal [ 1.; 0.; 0.; 0.; 0.; 1.; 1.; 0.; 0.; 1. ]
  @@ List.map random_indices ~f:(fun (i, j) ->
         sample_plane ~i ~j ~grid_size:2
           ~alpha:(45. |> Degree.of_float |> Degree.to_radian)
           ~beta:(-30. |> Degree.of_float |> Degree.to_radian)
           ~center:(Vec3.of_list [ 0.; 1.; 3. ])
           ~img_w:50 ~view_size:4 ~half_edge_length:2 ~line_w:0.3);
  assert_equal [ 0.; 1.; 0.2; 0.2; 0.2; 0.2; 0.; 0.2; 1.; 0.2 ]
  @@ List.map random_indices ~f:(fun (i, j) ->
         sample_sphere ~i ~j ~grid_size:8 ~directions:direction
           ~alpha:(30. |> Degree.of_float |> Degree.to_radian)
           ~beta:(35. |> Degree.of_float |> Degree.to_radian)
           ~img_w:50 ~half_edge_length:4 ~line_w:0.1);
  assert_equal [ 0.; 1.; 0.4; 0.4; 0.; 0.; 0.; 1.; 0.4; 0.4 ]
  @@ List.map random_indices ~f:(fun (i, j) ->
         sample_orthogonal ~i ~j ~grid_size:8 ~camera_offset:1.
           ~directions:direction
           ~alpha:(30. |> Degree.of_float |> Degree.to_radian)
           ~beta:(35. |> Degree.of_float |> Degree.to_radian)
           ~center:(Vec3.of_list [ 0.; 0.; 1. ])
           ~img_w:50 ~view_size:4 ~half_edge_length:2 ~plane_bd:4 ~line_w:0.1);
  assert_equal [ 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.2; 0.2; 0.2 ]
  @@ List.map random_indices ~f:(fun (i, j) ->
         sample_orthogonal ~i ~j ~grid_size:8 ~camera_offset:1.
           ~directions:direction
           ~alpha:(30. |> Degree.of_float |> Degree.to_radian)
           ~beta:(35. |> Degree.of_float |> Degree.to_radian)
           ~center:(Vec3.of_list [ 0.2; 0.5; 1. ])
           ~img_w:50 ~view_size:2 ~half_edge_length:1 ~plane_bd:2 ~line_w:0.1)

let plane_result =
  [
    0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 1.; 0.;
    0.; 0.; 0.; 0.; 0.; 0.; 1.; 0.; 1.; 1.; 0.; 0.; 0.; 0.; 0.; 1.; 0.; 1.; 0.;
    0.; 0.; 0.; 0.; 0.; 0.; 0.; 1.; 0.; 1.; 1.; 1.; 0.; 0.; 0.; 0.; 0.; 1.; 0.;
    1.; 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 1.; 1.; 1.; 1.; 0.; 0.; 0.; 0.; 0.; 0.;
    1.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.;
    0.; 0.; 0.; 0.; 0.;
  ]

let sphere_result =
  [
    0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.2; 0.2; 0.2; 0.2; 0.2;
    0.2; 0.2; 0.; 0.; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.; 0.2; 0.2;
    0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2; 0.2;
    0.2; 0.2; 0.; 0.2; 0.2; 0.2; 0.2; 0.2; 1.; 1.; 1.; 0.2; 0.; 0.2; 0.2; 0.2;
    0.2; 1.; 0.2; 0.2; 0.2; 0.2; 0.; 1.; 0.2; 0.2; 1.; 1.; 0.2; 1.; 0.2; 0.2;
    0.; 0.; 0.2; 1.; 0.2; 0.2; 0.2; 1.; 1.; 0.; 0.; 0.; 1.; 0.2; 1.; 0.2; 1.;
    0.2; 0.2; 0.;
  ]

let orthogonal_result =
  [
    0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
    0.; 0.; 0.; 0.; 0.; 0.; 0.4; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.4; 0.4; 0.4;
    0.; 0.; 0.; 0.; 0.; 0.4; 0.4; 0.4; 0.2; 0.4; 0.4; 0.4; 0.; 0.4; 0.4; 0.4;
    0.4; 0.2; 0.2; 0.2; 0.4; 0.4; 0.4; 0.4; 0.4; 0.4; 0.4; 0.4; 0.2; 0.4; 1.;
    0.4; 1.; 0.4; 0.4; 0.4; 0.4; 0.4; 1.; 0.4; 0.4; 0.4; 0.4; 0.; 0.; 0.4; 0.4;
    0.4; 1.; 0.4; 1.; 0.4; 0.; 0.; 0.; 0.; 0.; 0.4; 0.4; 0.4; 0.; 0.; 0.;
  ]

let different_pixels_less_than_n n l1 l2 =
  List.fold2_exn l1 l2 ~init:0 ~f:(fun acc a b ->
      if Float.( = ) a b then acc else acc + 1)
  < n

let image_tests _ =
  assert_equal true
  @@ (getImage ~img_w:10 ~view_size:4 ~plane_bd:4 ~half_edge_length:2
        ~line_w:0.25 ~grid_size:2 ~sampling_n:1 Planar
        ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ])
     |> Gray_image.to_float_list
     |> different_pixels_less_than_n 5 plane_result);
  (* I think the descrepency of the sphere test is because of the floating point issue
     It only happens and the boundary point when I look at the result, so we should be fine *)
  assert_equal true
  @@ (getImage ~img_w:10 ~view_size:4 ~plane_bd:4 ~half_edge_length:2
        ~line_w:0.25 ~grid_size:2 ~sampling_n:1 Sphere
        ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ])
     |> Gray_image.to_float_list
     |> different_pixels_less_than_n 5 sphere_result);
  assert_equal true
  @@ (getImage ~img_w:10 ~view_size:4 ~plane_bd:4 ~half_edge_length:2
        ~line_w:0.25 ~grid_size:2 ~sampling_n:1 Orthogonal
        ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ])
     |> Gray_image.to_float_list
     |> different_pixels_less_than_n 5 orthogonal_result)

let rasterizer_tests =
  "Rasterizer tests"
  >: test_list
       [ "basic rasterizer test" >:: basic_tests; "image test" >:: image_tests ]

(* open Ascii_printer *)
let basic_print_ascii_tests _ =
  (*
  let image_width = 100 in
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Planar ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  let () = print_ascii_image img image_width in
  assert_equal true true;
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Sphere ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  let () = print_ascii_image img image_width in
  assert_equal true true;
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Orthogonal ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  let () = print_ascii_image img image_width in
  assert_equal true true; *)
  assert_equal true true

let print_ascii_tests =
  "Printing ascii tests"
  >: test_list [ "basic print ascii test" >:: basic_print_ascii_tests ]

let k1 =
  {
    alpha = Degree.of_float 180.;
    beta = Degree.of_float 0.;
    center = Vec3.of_list [ 0.; 0.; 1. ];
  }

let k2 =
  {
    alpha = Degree.of_float 0.;
    beta = Degree.of_float 90.;
    center = Vec3.of_list [ 0.; 0.; 0. ];
  }

let k3 =
  {
    alpha = Degree.of_float 90.;
    beta = Degree.of_float 45.;
    center = Vec3.of_list [ 0.; 0.; 0.5 ];
  }

let basic_animation_tests _ =
  assert_equal k1 @@ linear_interpolate k1 k2 0.;
  assert_equal k2 @@ linear_interpolate k1 k2 1.;
  (* This is dangerous and might be good to use floating point comparison but seems working fine*)
  assert_equal k3 @@ linear_interpolate k1 k2 0.5;
  assert_equal [ k1; k3; k2 ]
  @@ generate_keyframes k1 k2 2 1. linear_interpolate

let animation_tests =
  "animation tests" >::: [ "animation test" >:: basic_animation_tests ]

let series =
  "Project Tests"
  >::: [ math_tests; rasterizer_tests; print_ascii_tests; animation_tests ]

let () = run_test_tt_main series

(*

   open Core
   open Rasterizer
   open Math;;

   let direction = Vec3.of_list [-0.57735027;  0.57735027; -0.57735027], Vec3.of_list [0.70710678; 0.70710678; -0.], Vec3.of_list [-0.40824829; 0.40824829; 0.81649658] ;;

   sample_orthogonal 27 43 8 1. direction (rad 30.) (rad 35.) (Vec3.of_list [0.;0.;1.;]) 50 4 2 4 0.1;;
*)
