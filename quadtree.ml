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
(* Vérifie si une boule b déborde sur une surface surf. Cette fonction n'est pas exacte, elle
correspond à une approximation (elle serait exacte si la boule était carrée...). On se satisfera
de cette approximation, qui de toute façon est suffisante pour nos utilisations à venir. *)

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
  let n = int_of_float (log (max (xm/.(2.*.r)) (ym/.(2.*.r))) /. (log 2.)) in
(* On veut qu'une feuille du quadtree ne puisse contenir au plus que 4 boules. n est le nombre de découpages en deux
de la fenêtre graphique nécessaires pour obtenir des cases assez petites pour que cette propriété soit vérifiée. *)
  let rec aux n surf l = let (xmin, ymin, xmax, ymax) = surf in
    if (n = 0 || l = []) then F((surf), (boule_list_intersect surf l))
(* Quand n=0, on a atteint la taille d'une feuille. Si l = [], on n'a pas de boules dans la surface, il est donc
inutile de subdiviser davantage. *)
    else let x1 = (xmin +. xmax)/.2. and y1 = (ymin +. ymax)/.2. in
	 let surf4 = (x1, ymin, xmax, y1)
	 and surf3 = (x1, y1, xmax, ymax)
	 and surf1 = (xmin, ymin, x1, y1)
	 and surf2 = (xmin, y1, x1, ymax) in
	 N(surf, 
	   aux (n-1) surf1 (boule_list_intersect surf1 l), 
	   aux (n-1) surf2 (boule_list_intersect surf2 l), 
	   aux (n-1) surf3 (boule_list_intersect surf3 l), 
	   aux (n-1) surf4 (boule_list_intersect surf4 l)) in
  let surf0 = (0., 0., xm, ym) in
aux n surf0 l;;

let rec union l1 l2 =
if l2 = [] then l1
else match l1 with
  |[] -> l2
  |x::r -> if List.mem x l2 then union r l2
    else x :: (union r l2);;

let rec find_boules_adjacentes b = function
  |F(surf, l) -> if mem_surface b surf then l else []
  |N(surf, t1, t2, t3, t4) -> if not (mem_surface b surf) then []
    else union (union (union (find_boules_adjacentes b t1) (find_boules_adjacentes b t2)) (find_boules_adjacentes b t3)) (find_boules_adjacentes b t4);;
(* Renvoie la liste des boules qui partagent une surface du quadtree avec b *)

let rec list_except x = function
  |[] -> []
  |a::r -> if a=x then r else a::(list_except x r);;
(* Supprime la première apparition de x dans une liste. *)

let rec erase_boule_from_quadtree b = function
  |F(surf, l) -> if mem_surface b surf then F(surf, list_except b l) else F(surf,l)
  |N(surf, t1, t2, t3, t4) -> if mem_surface b surf then
      N(surf, erase_boule_from_quadtree b t1,  erase_boule_from_quadtree b t2,  erase_boule_from_quadtree b t3,  erase_boule_from_quadtree b t4)
    else N(surf, t1, t2, t3, t4);;
(* Supprime les apparitions de b dans un quadtree. *)

let quadtree_except b t = t:=(erase_boule_from_quadtree b !t);;
(* Remplace une référence de quadtree par le quadtree sans apparition de b *)

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

let rec gere_contact b = function
  | [] -> ()
  | x::r -> if x = b then ()
    else if contact b x then 
      if not (search_contact_triple b x r) then collision b x
      else ()
    else ();
    gere_contact b r;;
(* Gère toute sorte de contact possible entre la boule b et les boules d'une liste. *)

let evolution_with_quadtree bill quadtree =
  let tree = ref quadtree in
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
  quadtree_except b tree;
  let l = find_boules_adjacentes b !tree in
  gere_contact b l
(* On ne veut tester qu'une seule fois chaque collision, c'est pourquoi on retire la boule du quadtree avant de tester ses collisions. *)
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

let draw_quadtree t = Graphics.set_color Graphics.black;
  let rec aux = function
    |F(surf,l) -> let (xmin, ymin, xmax, ymax) = surf in
		Graphics.draw_rect (int_of_float xmin) (int_of_float ymin) (int_of_float(xmax-.xmin)) (int_of_float(ymax-.ymin))
    |N(surf,t1,t2,t3,t4) -> aux t1; aux t2; aux t3; aux t4 in
aux t;;

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
  draw_trou bill.trous.(i) 
done;
for i = 1 to n-1 do
  draw_boule m.(i) 
done;
draw_boule0 m.(0);;

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
