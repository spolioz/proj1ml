open Vecteur;;

let f1 = 0.995 ;;
(* On multiplie la vitesse par f1 après chaque instant dt, ce qui traduit les frottements. *)
let dt = 0.01 ;;
(* dt sera la frame choisie pour l'affichage séquentiel. *)

type boule2 = {mutable o : vect; r : float; mutable s : float; mutable v : vect; mutable a : vect;};;
(* On repère une boule par son centre o, son rayon r, sa solidité s, sa vitesse v, et son accélération a. *)

let vitesse b1 = sqrt (carre b1.v.x +. carre b1.v.y);;
(* Donne la norme de la vitesse d'une boule. *)

let contact b1 b2 = b1<>b2 && distance b1 b2 < (b1.r +. b2.r);;
(* Détermine si oui ou non deux boules sont en contact strict. *)

let exists_contact b niv = let i = ref 0 in 
while (!i < niv.n && not (contact b niv.boules.(!i))) do incr i done;
!i < niv.n;;

let evolution_boule b niv =
  let v = norme b.v in
b.v.x <- b.v.x *. f1 +. (dt *. b.a.x);
b.v.y <- b.v.y *. f1 +. (dt *. b.a.y);
b.o.x <- b.o.x +. (dt *. b.v.x);
b.o.y <- b.o.y +. (dt *. b.v.y);
b.a.x <- 0. (*-.v*.b.v.x*);
if not (exists_contact b niv) then b.a.y <- g (*-. v*.b.v.y*)
;;
(* Fait avancer les boules durant l'intervalle de temps dt, 
selon leur vitesse, sans prendre en compte les collisions. *)

let separe b1 b2 = 
  let bary = mult_scalaire (1./.(b1.r +. b2.r)) (add_vect (mult_scalaire b2.r b1.o) (mult_scalaire b1.r b2.o)) in
  let u = vect_dir b1 b2 in
b1.o <- add_vect bary (mult_scalaire (-. b1.r) u);
b2.o <- add_vect bary (mult_scalaire (b2.r) u);;

let reaction_support b1 b2 = 
if b1.a.y < b2.a.y then 
  let u = vect_dir b1 b2 in
  b2.a <- add_vect b2.a (mult_scalaire (scalaire b2.a u) u)
else
  let u = vect_dir b2 b1 in
  b1.a <- add_vect b1.a (mult_scalaire (scalaire b1.a u) u);;
(* Traduit la compensation de l'accélération d'une boule selon la 
réaction d'une boule support (d'accélération nulle) avec laquelle
elle serait en contact. *)


let collision b1 b2 = 
separe b1 b2;
  let u = vect_dir b1 b2 in
  let dv = mult_scalaire (scalaire (sous_vect b1.v b2.v) u) u in
  let ds = (norme dv)/.100. in
b2.v <- add_vect b2.v dv;
b1.v <- sous_vect b1.v dv;
reaction_support b1 b2;
if ds > 300. then begin
b2.s <- b2.s -. ds;
b1.s <- b1.s -. ds
end;;
(* Gère la collision entre deux boules (uniquement si elles sont en contact!) *)

let is_destroyed b = 
  let xm = float_of_int (Graphics.size_x()) in
b.s <= 0. || b.o.x < -.b.r || b.o.x > b.r +. xm;;
(* Indique si oui ou non une boule a été détruite *)
