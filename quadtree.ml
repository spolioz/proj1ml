open Vecteur;;
open Trou;;
open Boule;;
open Billard;;
open Graphics;;
open Lancement;;
open Graphique;;
open Interface;;


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

let quadtree_create l =
  let r = rayon_max l in
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
  let n = int_of_float (max (xm/.(2.*.r)) (ym/.(2.*.r))) in
(* On veut qu'une feuille du quadtree ne puisse contenir au plus que 4 boules. *)
  let rec aux n surf l = let (xmin, ymin, xmax, ymax) = surf in
    if (n = 0 || l = []) then F((surf), (boule_list_intersect surf l))
(* Quand n=0, on a atteint la taille d'une feuille. Si l = [], on n'a pas de boules dans la surface, il est donc
inutile de subdiviser davantage. *)
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
aux n surf0 l;;

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

let rec list_of_array t n = 
if n = 0 then []
else t.(n-1) :: (list_of_array t (n-1));;
(* Crée une liste à partir des n premiers éléments de t (n doit être inférieur à la taille de t!) *)

let build_quadtree bill = 
  let l = list_of_array bill.boules bill.n in
quadtree_create l;;

let search_contact_triple b1 b2 =
  let rec aux test = function
  | [] -> test
  | b3::r -> if contact_triple b1 b2 b3 then (collision_triple b1 b2 b3; true)
    else aux test r
  in aux false;;
(* Gère un potentiel contact triple possible entre b1, b2, et une boule d'une liste.
Renvoie true si une telle collision a eu lieu. *)

let gere_contact b = function
  | [] -> ()
  | x::r -> List.iter (fun x -> if contact b x then if not (search_contact_triple b x r) then collision b x) r;;
(* Gère toute sorte de contact possible entre la boule b et les boules d'une liste. *)

let evolution_with_quadtree bill tree =
  let rep = ref false in
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
bill.trous.(1).o.x <- xm;
bill.trous.(2).o.y <- ym;
bill.trous.(3).o.x <- xm;
bill.trous.(3).o.y <- ym;
bill.trous.(4).o.x <- xm/.2.;
bill.trous.(5).o.x <- xm/.2.;
bill.trous.(5).o.y <- ym;
  let m = bill.boules in
  let n = bill.n in
  let t = Sys.time() in
for i = 0 to n-1 do
  evolution_boule m.(i)
  (* On commence par faire avancer les boules selon leur vitesse. *)
done;
for i = 0 to n-1 do
  let dir = out_of_billard m.(i) in 
  if dir <> Nil 
  then ricochet m.(i) dir
  (* La boule sort du billard, il faut donc la faire rebondir contre le bord. *)
done;
for i = 1 to n-1 do
  let b = m.(i) in
  let l = find_boules_adjacentes b tree in
  gere_contact b l
done;
let n1 = Array.length bill.trous in
let i = ref 0 in
while !i < bill.n do
(* Au fur et à mesure que l'on supprime des boules, la valeur de bill.n va
être modifiée : c'est pour cela que l'on utilise une boucle while, qui teste
donc à chaque itération la nouvelle valeur de bill.n. *)
  for j = 0 to n1-1 do
    if tombe_trou m.(!i) bill.trous.(j) 
    then (if !i = 0 then rep := true; supprime_boule !i bill)
    (* On supprime les boules qui sont tombées dans un trou. *)
    else if contact_trou m.(!i) bill.trous.(j) 
    then interaction_trou m.(!i) bill.trous.(j)
    (* On fait subir l'attraction du trou aux boules qui le touchent. *)
  done;
  incr i
done;
while (Sys.time() -. t < dt) do () done;
(* On attend dt, afin que l'affichage séquentiel suive la frame. *)
!rep
;;

let rec draw_quadtree = function
  |F(surf,l) -> let (xmin, ymin, xmax, ymax) = surf in
		Graphics.draw_rect (int_of_float xmin) (int_of_float xmax) (int_of_float(xmax-.xmin)) (int_of_float(ymax-.ymin))
  |N(surf,t1,t2,t3,t4) -> draw_quadtree t1; draw_quadtree t2; draw_quadtree t3; draw_quadtree t4;;

let draw_billard_with_quadtree bill tree = 
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
  let m = bill.boules in
  let n = bill.n in
reset();
Graphics.set_color vert;
Graphics.fill_rect 0 0 xm ym;
draw_quadtree tree;
let n1 = Array.length bill.trous in
for i = 0 to n1-1 do 
  draw_trou bill.trous.(i) done;
for i = 1 to n-1 do
  draw_boule m.(i) 
done;
draw_boule0 m.(0);
;;

let launch_with_quadtree bill = 
  let rep = ref false in
draw_billard bill;
while (not !rep && vit_max bill > 30. && bill.n > 1) do
  let tree = build_quadtree bill in
  rep :=  evolution_with_quadtree bill tree;
  Graphics.auto_synchronize false;
  draw_billard_with_quadtree bill tree;
  Graphics.auto_synchronize true;
done;
!rep;;

let partie_with_quadtree bill =
  let b = copy_boule bill.boules.(0) in
while bill.n > 1 do
lance_boule bill;
if launch_with_quadtree bill then (insert_boule b bill; place_boule bill) done;;
