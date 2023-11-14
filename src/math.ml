open Core

(* basic types *)
module type Vec = (* N dimensional vector*)
sig
  type t
  val n : int
  val nth: t -> int -> float
  val set: t -> int -> float -> unit
  val to_list: t -> float list
  val of_list: float list -> t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): float -> t -> t (* scaler multiplycation *)
  val sqr_length: t -> float
  val length: t -> float
  val unit: t -> t
  val dot: t -> t -> float
end

module MakeNVec (X : sig val n : int end) : Vec = struct 
  type t = float array

  let n = X.n
  let nth = Array.get
  let set = Array.set
  let to_list = Array.to_list
  let of_list l = if List.length l = n then Array.of_list l else failwith "MakeNVec.of_list: length don't match!"
  let ( + ) v1 v2 = Array.map2_exn v1  v2 ~f:(+.)
  let ( - ) v1 v2 = Array.map2_exn v1  v2 ~f:(-.)
  let ( * ) s v = Array.map v ~f:(fun x->x *. s)
  let sqr_length v = Array.fold v ~init:0. ~f:(fun acc x-> acc +. x *. x)
  let length v = sqrt @@ sqr_length v
  let unit (v:t) : t = (1. /. length v) * v 
  let dot (v1:t) (v2:t) : float = Array.fold2_exn v1 v2 ~init:0. ~f:(fun acc x y-> acc +. x *. y)

end


module Vec2 : Vec = MakeNVec ( struct let n = 2 end )
module Vec3 : Vec = MakeNVec ( struct let n = 3 end )

(* helper functions *)
let rad x = x *. Float.pi /. 180. (* degree to radian *)

let cross v1 v2 = Vec3.of_list [ (Vec3.nth v1 1) *. (Vec3.nth v2 2) -. (Vec3.nth v1 2) *. (Vec3.nth v2 1);
                                 (Vec3.nth v1 2) *. (Vec3.nth v2 0) -. (Vec3.nth v1 0) *. (Vec3.nth v2 2); 
                                 (Vec3.nth v1 0) *. (Vec3.nth v2 1) -. (Vec3.nth v1 1) *. (Vec3.nth v2 0) ]


(* compute the projection between x^2+y^2+(z-z_c)^2=1 sphere to the z=0 plane, requires z_c > 0 
   input and output are all global coordinates
*) 
let sProj v3 ~z_c= 
  if Float.compare 0. z_c = -1 then
    let x = Vec3.nth v3 0 and y = Vec3.nth v3 1 and z = Vec3.nth v3 2 in
    Vec2.of_list [(z_c +. 1.) *. x /. (z_c+.1. -.z); (z_c +. 1.) *. y /. (z_c+.1. -.z)]
  else failwith "stereoProj: zc should not be a negative value" 

(* inverse transform of previous function *)
let invSProj v2 ~z_c = 
  if Float.compare 0. z_c = -1 then
    let x = Vec2.nth v2 0 and y = Vec2.nth v2 1 and z_c1 = z_c +. 1. in 
    let a = 1. /. (z_c1 *. z_c1 +. x *. x +. y *. y) in
    Vec3.of_list [2. *. z_c1 *. a *. x; 2. *. z_c1 *. a *. y; z_c -. (z_c1 *. z_c1 -. x *. x -. y *. y) *. a ] 
  else failwith "stereoProj: zc should not be a negative value" 


(* this part construct the R^2 -> R^2  function such that given (X,Y), we should sample from (X',Y') which is the location given by inverse Moebius transformation *)
(* step 1. invSProj (X,Y) -> global (x1, y1, z1) on S2 centered at (0,0,z_c) -> local (x1_, y1_, z1_) on S2 *) 
let map2localS2 center v2 = 
  let x_c = Vec3.nth center 0 and y_c = Vec3.nth center 1 and z_c = Vec3.nth center 2 in
  let v2_centered = Vec2.(-) v2 (Vec2.of_list [x_c; y_c]) in 
  let v3global = invSProj v2_centered ~z_c in  
  Vec3.(-) v3global (Vec3.of_list [0.;0.;z_c]) 

(* step 2. rotate and translate back, S2 at origin -> S2 at (0,0,1) *)
(* rotation along z-axis for -alpha angle*)
let r_alpha alpha v3 = 
  let x = Vec3.dot (Vec3.of_list [Float.cos(-.alpha); Float.sin(alpha); 0.]) v3 and 
      y = Vec3.dot (Vec3.of_list [Float.sin(-.alpha); Float.cos(-.alpha); 0.]) v3 and
      z = Vec3.dot (Vec3.of_list [0.; 0.; 1.]) v3 in 
  Vec3.of_list [x; y; z]

(* rotation along y-axis for beta angle *)
let r_beta beta v3 = 
  let x = Vec3.dot (Vec3.of_list [Float.cos(beta); 0.; Float.sin(beta)]) v3 and 
      y = Vec3.dot (Vec3.of_list [0.; 1.; 0.]) v3 and
      z = Vec3.dot (Vec3.of_list [-.Float.sin(beta); 0.; Float.cos(beta)]) v3 in 
  Vec3.of_list [x; y; z]

(* step 3. sProj (x,y,z) -> (X', Y') which is the location on z-plane to sample from *)
(* wrapping all steps up*)
let mapMoebius v2 ~alpha ~beta ~center = 
  v2 |> map2localS2 center |> r_alpha alpha |> r_beta beta |> Vec3.(+) (Vec3.of_list [0.;0.;1.]) |> sProj ~z_c:1.

