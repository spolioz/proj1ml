open Vecteur;;
open Boule;;


let contact_trou b trou = distance b trou < trou.r;;
(* Indique si oui ou non une boule est en contact avec un trou. *)

let tombe_trou b trou = distance b trou < (trou.r -. b.r);;
(* Indique si oui ou non une boule est en position pour tomber dans un trou. *)

let interaction_trou b trou = 
  let u = vect_dir b trou in
  let d = (trou.r -. (distance b trou))/.trou.r in
  let a = 100000.*. d *. sqrt (1. -. (carre d)) in
b.a <- mult_scalaire a u;;
(* Fait subir l'attraction d'un trou à une boule. *)
