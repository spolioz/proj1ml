open Vecteur;;
open Boule;;
open Billard;;

type surface = float * float * float * float;;
(* Une surface est un rectangle repéré par ses 
abscisses et ordonnées minimales et maximales. *)

type quadtree = F of surface*(boule list) | N of surface*quadtree*quadtree*quadtree*quadtree;;
(* Un quadtree renseigne ses subdivision par structure arborescente, ainsi que la liste
des boules intersectant chaque subdivision. *)

let rec rayon_max = function
  | [] -> 0.
  | b :: r -> max b.r (rayon_max r);;
(* Calcule le rayon maximal d'une liste de boules. *)

let mem_surface b surf = let (xmin, ymin, xmax, ymax) = surf in
b.o.x +. b.r > xmin && b.o.x -. b.r < xmax && b.o.y +. b.r > ymin && b.o.y -. b.r < ymax;;

let surf = function
  |F(surf, l) -> surf
  |N(surf, t1, t2, t3, t4) -> surf;;

let rec boule_list_intersect surf = function
  | [] -> []
  | b :: r -> if (mem_surface b surf)
    then b :: (boule_list_intersect surf r)
    else boule_list_intersect surf r;;

let rec quadtree_create l =
  let r = rayon_max l in
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
  let n = int_of_float (max (xm/.(4.*.r)) (ym/.(4.*.r))) in
  let rec aux n surf l = let (xmin, ymin, xmax, ymax) = surf in
    if n = 0 then F((surf), (boule_list_intersect surf l))
    else let x1 = (xmin +. xmax)/.2. and y1 = (ymin +. ymax)/.2. in
	 let surf4 = (x1, xmax, ymin, y1)
	 and surf3 = (x1, xmax, y1, ymax)
	 and surf1 = (xmin, x1, ymin, y1)
	 and surf2 = (xmin, x1, y1, ymax) in
	 N(surf, 
	   aux (n-1) surf1 (boule_list_intersect surf1 l), 
	   aux (n-1) surf2 (boule_list_intersect surf2 l), 
	   aux (n-1) surf3 (boule_list_intersect surf3 l), 
	   aux (n-1) surf4 (boule_list_intersect surf4 l)) in
  let surf0 = (0., 0., xm, ym) in
aux n surf0;;

let rec concat_without_x x l1 l2 = match l1 with
  | [] -> l2
  | a::r -> if a = x then concat_without_x x r l2
    else a::(concat_without_x x r l2);;
(* Concatène une liste à une autre en supprimant les apparitions de x dans la première. *)

let rec find_boules_adjacentes b = function
  |F(surf, l) -> if mem_surface b surf then l else []
  |N(surf, t1, t2, t3, t4) -> if not (mem_surface b surf) then []
    else concat_without_x b (concat_without_x b (concat_without_x b (concat_without_x b (find_boules_adjacentes b t1) (find_boules_adjacentes b t2)) (find_boules_adjacentes b t3)) (find_boules_adjacentes b t4)) []
;;
(* Renvoie la liste des boules qui partagent une surface du quadtree avec b *)

let iter3 l1 l2 = List.iter (List.iter2 l1 l2);;
