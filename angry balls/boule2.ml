open Vecteur;;

let f1 = 0.995 ;;
(* On multiplie la vitesse par f1 après chaque instant dt, ce qui traduit les frottements. *)
let dt = 0.01 ;;
(* dt sera la frame choisie pour l'affichage séquentiel. *)
let g = -. 1000.;;
(* La gravité. *)

type boule2 = {mutable o : vect; r : float; mutable s : float; mutable v : vect; mutable a : vect;};;
(* On repère une boule par son centre o, son rayon r, sa solidité s, sa vitesse v, et son accélération a. *)

let copy_boule b = {o = {x = b.o.x; y = b.o.y}; r = b.r; s = b.s; v = {x = b.v.x; y = b.v.y}; a = {x = b.a.x; y = b.a.y};};;
(* Effectue la copie du contenu d'une boule. *)

let distance b1 b2 = norme (sous_vect b2.o b1.o);;
(* Donne la distance entre les centres de deux boules. *)

let vitesse b1 = norme b1.v;;
(* Donne la norme de la vitesse d'une boule. *)

let vect_dir b1 b2 = let d = distance b1 b2 in
mult_scalaire (1. /. d) (sous_vect b2.o b1.o);;
(* Renvoie le vecteur directeur unitaire qui relie deux boules. *)


let contact b1 b2 = b1<>b2 && distance b1 b2 <= (b1.r +. b2.r);;
(* Détermine si oui ou non deux boules sont en contact strict. *)

let evolution_boule b niv =
  let v = norme b.v in
b.o.x <- b.o.x +. (dt *. b.v.x);
b.o.y <- b.o.y +. (dt *. b.v.y);
b.v.x <- b.v.x *. f1 +. (dt *. b.a.x);
b.v.y <- b.v.y *. f1 +. (dt *. b.a.y);
b.a.x <- 0.;
b.a.y <- if b.o.y <= 1.1 *. b.r then 0. 
(* Dans le cas où la boule est posée sur le sol (ou dans une approximation assez proche), la réaction du sol compense son accélération dûe à la gravité. *)
else g (*-. v*.b.v.y*);;
(* Fait avancer les boules durant l'intervalle de temps dt, 
selon leur vitesse, sans prendre en compte les collisions. *)

let separe b1 b2 = 
  let bary = mult_scalaire (1./.(b1.r +. b2.r)) (add_vect (mult_scalaire b2.r b1.o) (mult_scalaire b1.r b2.o)) in
  let u = vect_dir b1 b2 in
b1.o <- add_vect bary (mult_scalaire (-. b1.r) u);
b2.o <- add_vect bary (mult_scalaire (b2.r) u);;

let reaction_support b1 b2 =
let u = vect_dir b1 b2 in
let g = {x = 0.; y = g;} in
if b1.a.y < b2.a.y then
  b2.a <- add_vect g (mult_scalaire (-. scalaire g u) u)
else
  b1.a <- add_vect g (mult_scalaire (-. scalaire g u) u)
;;
(* Traduit la compensation de l'accélération d'une boule selon la 
réaction d'une boule support (d'accélération nulle) avec laquelle
elle serait en contact. *)

let is_destroyed b = 
  let xm = float_of_int (Graphics.size_x()) in
b.s <> infinity && (b.s <= 0. || b.o.x < -.b.r || b.o.x > b.r +. xm);;
(* Indique si oui ou non une boule a été détruite *)
