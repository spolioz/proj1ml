open Vecteur;;

let f1 = 0.995 ;;
(* On multiplie la vitesse par f1 après chaque instant dt, ce qui traduit les frottements. *)
let dt = 0.01 ;;
(* dt sera la frame choisie pour l'affichage séquentiel. *)


type boule = {mutable o : vect; r : float; m : float; mutable v : vect; mutable a : vect;};;
(* On repère une boule par son centre o, son rayon r, sa masse m, sa vitesse v, et son accélération a. *)

let copy_boule b = {o = {x = b.o.x; y = b.o.y}; r = b.r; m = b.m; v = {x = b.v.x; y = b.v.y}; a = {x = b.a.x; y = b.a.y};};;

let carre x = x *. x;;
let distance b1 b2 = sqrt (carre (b1.o.x -. b2.o.x) +. carre (b1.o.y -. b2.o.y));;
(* Donne la distance entre les centres de deux boules. *)
let vitesse b1 = sqrt (carre b1.v.x +. carre b1.v.y);;
(* Donne la norme de la vitesse d'une boule. *)

let vect_dir b1 b2 = let d = distance b1 b2 in
{x = (b2.o.x -. b1.o.x) /. d ; y = (b2.o.y -. b1.o.y) /. d};;
(* Renvoie le vecteur directeur unitaire qui relie deux boules. *)

let contact b1 b2 = distance b1 b2 <= (b1.r +. b2.r);;
(* Détermine si oui ou non deux boules sont en contact strict. *)

let contact_triple b1 b2 b3 = 
distance b1 b2 <= (b1.r +. b2.r)
&& distance b1 b3 <= (b1.r +. b3.r)
&& distance b2 b3 <= (b2.r +. b3.r);;

let evolution_boule b =
b.v.x <- b.v.x *. f1 +. (dt *. b.a.x);
b.v.y <- b.v.y *. f1 +. (dt *. b.a.y);
b.o.x <- b.o.x +. (dt *. b.v.x);
b.o.y <- b.o.y +. (dt *. b.v.y);
b.a.x <- 0.;
b.a.y <- 0.;;
(* Fait avancer les boules durant l'intervalle de temps dt, 
selon leur vitesse, sans prendre en compte les collisions. *)

let separe b1 b2 = 
  let bary = mult_scalaire (1./.(b1.r +. b2.r)) (add_vect (mult_scalaire b2.r b1.o) (mult_scalaire b1.r b2.o)) in
  let u = vect_dir b1 b2 in
b1.o <- add_vect bary (mult_scalaire (-. b1.r) u);
b2.o <- add_vect bary (mult_scalaire (b2.r) u);;
(* Utilisée dans le cas de boule qui s'intersectent, cette fonction les sépare
en les translant selon leur vecteur directeur, de sorte qu'elle soient juste en
contact. Le centre de masse des deux boules reste inchangé. *)

let collision b1 b2 = 
separe b1 b2;
  let u = vect_dir b1 b2 in
  let dv = mult_scalaire (scalaire (sous_vect b1.v b2.v) u) u in
b2.v <- add_vect b2.v dv;
b1.v <- sous_vect b1.v dv;;
(* Gère la collision entre deux boules (uniquement si elles sont en contact!) *)

let collision_triple b1 b2 b3 = 
separe b1 b2;
separe b2 b3;
separe b1 b3;
  let u = vect_dir b1 b2 in
  let v = vect_dir b2 b3 in
  let w = vect_dir b1 b3 in
  let dvu = mult_scalaire (scalaire (sous_vect (mult_scalaire 0.5 b1.v) (mult_scalaire 0.5 b2.v)) u) u in
  let dvv = mult_scalaire (scalaire (sous_vect (mult_scalaire 0.5 b2.v) (mult_scalaire 0.5 b3.v)) v) v in
  let dvw = mult_scalaire (scalaire (sous_vect (mult_scalaire 0.5 b1.v) (mult_scalaire 0.5 b3.v)) w) w in
b1.v <- sous_vect (sous_vect b1.v dvu) dvw;
b2.v <- sous_vect (add_vect b2.v dvu) dvv;
b3.v <- add_vect (add_vect b3.v dvv) dvw;;
(* Gère une collision entre 3 boules. *)