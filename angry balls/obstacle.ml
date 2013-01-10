open Boule2;;
open Vecteur;;

type obstacle = float*float*float*float * boule2;;
(* Un obstacle est un mur fixe, planté dans le sol, repéré par ses coordonnées extrémales, et d'extrémité arrondie, représentée par une boule. *) 

type intersection = Mur | Boule | Nil;;
(* Il y a deux types d'intersection entre une boule et l'obstacle : soit avec
le mur, soit avec son extrémité arrondie. 
Nil correspond à une intersection inexistante.*)

let intersect_obstacle b ob = let (xmin,ymin,xmax,ymax,b1) = ob in
if b.o.y <= ymax then 
  if (b.o.x +. b.r >= xmin && b.o.x -. b.r <= xmax) then Mur else Nil
else if contact b b1 then Boule
else Nil;;
(* Renvoie le type d'intersection entre une boule et un obstacle (potentiellement inexistante). *)

type cote = Gauche | Droite;;
(* Une boule est soit à Gauche, soit à Droite d'un obstacle. *)

let quel_cote b ob = let (xmin,ymin,xmax,ymax,b1) = ob in
if b.o.x < (xmin +. xmax) /. 2. then Gauche else Droite;;
(* Définit de quel côté une boule se trouve par rapport à un obstacle. *)



let collision_obstacle_boule b1 ob = let (xmin,ymin,xmax,ymax,b2) = ob in
  let u = vect_dir b1 b2 in
  let dv = mult_scalaire (2. *. (scalaire (sous_vect b1.v b2.v) u)) u in
  let ds = (norme dv)*.4. in
  b1.o <- sous_vect b2.o (mult_scalaire (b1.r +. b2.r) u);
  b1.v <- sous_vect b1.v dv;
if ds > 800. then b1.s <- b1.s -. ds;;
(* Gère la collision entre l'extrémité arrondie d'un obstacle et une boule. *)

let collision_obstacle b ob = let test = intersect_obstacle b ob in
			      let (xmin,ymin,xmax,ymax,b1) = ob in
match test with
  |Mur -> if quel_cote b ob = Gauche then b.o.x <- xmin -. (1.01*.b.r) else b.o.x <- xmax +.(1.01*.b.r);
    b.v.x <- -. b.v.x
  |Boule -> collision_obstacle_boule b ob
  |Nil -> ();;
(* Gère la collision d'une boule sur un obstacle. *)

let make_obstacle x0 y0 l h = (x0, y0, x0 +. l, y0 +. h, 
{o = {x = x0 +. (l/.2.); y = y0 +. h}; r = l/.2.; s = infinity; v = {x = 0.;y = 0.}; a = {x = 0.; y = 0.}});;
(* Crée un obstacle de coin gauche en (x0,y0), de largeur l et de hauteur h. *)
