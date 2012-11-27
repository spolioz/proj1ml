#load "graphics.cma";;
Graphics.open_graph "";;

let g = -.1000.;;
(* g est la constante de gravitation utilisée dans le programme *)

let f1 = 0.995 ;;
(* On multiplie la vitesse par f1 après chaque instant dt, ce qui traduit les frottements. *)

let dt = 0.01 ;;
(* dt sera la frame choisie pour l'affichage séquentiel. *)

type vect = {mutable x : float; mutable y : float};;
(*un vecteur permettra de définir un point, un vitesse, une accélération, ...*)

type boule = {mutable o : vect; r : float; mutable s : float; mutable v : vect; mutable a : vect;};;
(* On repère une boule par son centre o, son rayon r, sa solidité s, sa vitesse v, et son accélération a. *)

let b1 = {o={x=200.;y=60.}; r=20.; s=8000.; v ={x=Random.float 3000. ;y=Random.float 3000.}; a = {x=0.; y=0.}};;
let b2 = {o={x=180.;y=180.}; r=20.; s=125000.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let b3 = {o={x=300.;y=300.}; r=20.; s=125000.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
type niveau = {boules : boule array; mutable n : int};;
let m = {boules = [|b1;b2;b3|]; n=3};;
(* L'exemple sur lequel on testera nos fonctions *)

let swap_tab i j tab = let x = tab.(j) in tab.(j) <- tab.(i); tab.(i) <- x;;
let supprime_boule i niv = let n = niv.n in
swap_tab i (n-1) (niv.boules); niv.n <- n-1;;

let carre x = x *. x;;
let distance b1 b2 = sqrt (carre (b1.o.x -. b2.o.x) +. carre (b1.o.y -. b2.o.y));;
(* Donne la distance entre les centres de deux boules. *)
let vitesse b1 = sqrt (carre b1.v.x +. carre b1.v.y);;
(* Donne la norme de la vitesse d'une boule. *)

let scalaire u v = u.x*.v.x +. u.y*.v.y;;
(* Implémentation du produit scalaire sur deux vecteurs. *)
let norme v = (sqrt (carre v.x)+.(carre v.y));;
(* Pour calculer la norme d'un vecteur. *)
let vect_dir b1 b2 = let d = distance b1 b2 in
{x = (b2.o.x -. b1.o.x) /. d ; y = (b2.o.y -. b1.o.y) /. d};;
(* Renvoie le vecteur directeur unitaire qui relie deux boules. *)
let add_vect u v = {x = u.x +. v.x; y = u.y +. v.y};;
(* addition sur les vecteurs. *)
let sous_vect u v = {x = u.x -. v.x; y = u.y -. v.y};;
(* Soustraction sur les vecteurs. *)
let mult_scalaire k v =  {x = k *. v.x; y = k *. v.y};;
(* Multiplication d'un vecteur par un scalaire flottant. *)

let evolution_boule b =
  let v = norme b.v in
b.v.x <- b.v.x *. f1 +. (dt *. b.a.x);
b.v.y <- b.v.y *. f1 +. (dt *. b.a.y);
b.o.x <- b.o.x +. (dt *. b.v.x);
b.o.y <- b.o.y +. (dt *. b.v.y);
b.a.x <- 0. (*-.v*.b.v.x*);
b.a.y <- g (*-. v*.b.v.y*)
;;
(* Fait avancer les boules durant l'intervalle de temps dt, 
selon leur vitesse, sans prendre en compte les collisions. *)

let contact b1 b2 = distance b1 b2 < (b1.r +. b2.r);;
(* Détermine si oui ou non deux boules sont en contact strict. *)

let separe b1 b2 = 
  let bary = mult_scalaire (1./.(b1.r +. b2.r)) (add_vect (mult_scalaire b2.r b1.o) (mult_scalaire b1.r b2.o)) in
  let u = vect_dir b1 b2 in
b1.o <- add_vect bary (mult_scalaire (-. b1.r) u);
b2.o <- add_vect bary (mult_scalaire (b2.r) u);;

let reaction_support b1 b2 = 
if b1.a.y = 0. then 
  let u = vect_dir b1 b2 in
  b2.a <- add_vect b2.a (mult_scalaire (scalaire b2.a u) u)
else if b2.a.y = 0. then
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
if ds > 100. then begin
b2.s <- b2.s -. ds;
b1.s <- b1.s -. ds
end;;
(* Gère la collision entre deux boules (uniquement si elles sont en contact!) *)

let is_destroyed b = 
  let xm = float_of_int (Graphics.size_x()) in
b.s <= 0. || b.o.x < -.b.r || b.o.x > b.r +. xm;;
(* Indique si oui ou non une boule a été détruite *)

type direction = Droite | Gauche | Haut | Bas | Nil;;

let out_of_billard b = 
if b.o.y -. b.r < 0. then Bas
else Nil;;
(* Renvoie Nil si la boule est à l'intérieur strictement du billard,
et sinon la direction dans laquelle elle commence à sortir du billard. *)

let ricochet b dir = match dir with
  |Bas -> b.v.y <- -. b.v.y; b.o.y <- b.r; b.s <- b.s -. 0.2*.(abs_float b.v.y); b.a.y <- 0.
  |_ -> ();;
(* Gère le rebond sur un bord de la fenêtre graphique (= du billard), 
en fonction de la direction d'échappement. *)

let evolution niv =
  let m = niv.boules in
  let n = niv.n in
  let t = Sys.time() in
for i = 0 to n-1 do
  evolution_boule m.(i)
  (* On commence par faire avancer les boules selon leur vitesse. *)
done;
for i = 0 to n-1 do
  let dir = out_of_billard m.(i) in 
  ricochet m.(i) dir
  (* La boule tombe dans le sol, il faut donc la faire rebondir. *)
done;
for i = 1 to n-1 do for j = 0 to i-1 do
  if contact m.(i) m.(j) 
  then collision m.(i) m.(j) 
  (* On fait rebondir les boules qui s'entrechoquent. *)
done done;
let i = ref 0 in
while !i < niv.n do
(* Au fur et à mesure que l'on supprime des boules, la valeur de bill.n va
être modifiée : c'est pour cela que l'on utilise une boucle while, qui teste
donc à chaque itération la nouvelle valeur de bill.n. *)
  if is_destroyed m.(!i) 
  then supprime_boule !i niv;
  (* On supprime les boules qui ont été détruites. *)
  incr i
done;
while (Sys.time() -. t < dt) do () done
(* On attend dt, afin que l'affichage séquentiel suive la frame. *)
;;
(* Fait passer la configuration d'un billard de l'instant t à l'instant t+dt. *)

let vit_max niv = let n = niv.n in
		   let m = niv.boules in
let max = ref (vitesse m.(0)) in
for i = 1 to n-1 do
  let v = vitesse m.(i) in 
  if v > !max then max := v
done;
!max;;
(* Renvoie la vitesse de la boule la plus rapide à l'intérieur du billard. *)


let reset() = Graphics.set_color Graphics.black;
Graphics.clear_graph();
Graphics.moveto 0 0;;
(* Réinitialise le graphe. *)

let draw_boule b = Graphics.set_color Graphics.red; 
Graphics.fill_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);
Graphics.set_color Graphics.black;
Graphics.draw_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
(* Pour dessiner une boule dans la fenêtre graphique. *)
let draw_niveau niv = 
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
  let m = niv.boules in
  let n = niv.n in
reset();
for i = 0 to n-1 do
  draw_boule m.(i) done
;;
(* Pour dessiner la configuration actuelle d'un niveau dans la fenêtre graphique. *)

let launch niv = 
draw_niveau niv;
while (vit_max niv >= 30. && niv.n > 0) do
  evolution niv; draw_niveau niv
done;;
(* Lance et affiche l'évolution du billard, tant que la vitesse des boules reste raisonnable. *)


(* Fonctions de création aléatoire utiles *)

let random_boule r = 
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
{o={x=Random.float (xm -.r); y=Random.float(ym-.r)}; 
r=r; s=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}};;

let rec random_boule_liste n r = 
if n=0 then []
else (random_boule r) :: (random_boule_liste (n-1) r);;

let make_niveau l =
if l = [] then failwith "Ce serait mieux d'avoir des boules à placer dans le niveau..."
else begin
  let r = (List.hd l).r in
  let m = Array.of_list l in
  let n = Array.length m in
  let niv = {boules = m; n=n} in
draw_niveau niv;
niv
end;;

let random_color() = Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255);;

let make_rangee_boule x n r =
  let n' = float_of_int n in
  let ymin = -.r in
     let rec aux i l = 
       if i = 0 then l 
       else aux (i-1) ({o={x=x; y=float_of_int(i)*.2.*.r +. ymin}; r=r; s=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}} :: l) in
aux (n) [];;

let make_triangle_boule n r =
  let xm = float_of_int (Graphics.size_x()) in
  let ym = float_of_int (Graphics.size_y()) in
  let rec aux i l = 
    if i=0 then (make_rangee_boule (xm -. 5.*.r) n r) @ l
    else aux (i-1) ((make_rangee_boule (xm -. 5.*.r -. (float_of_int i)*.2.*.r) (n-i) r) @ l) in
{o={x=5.*.r; y=ym/.2.}; r=r*.0.8; s=infinity; v={x=0.; y=0.}; a={x=0.; y=0.}}::(aux n []);;

let l = make_triangle_boule 6 20.;;

let m = make_niveau l;;
while m.n > 0 do
m.boules.(0).v.x <- (Random.float 1000.);
m.boules.(0).v.y <- (Random.float 1000.);
launch m done;;
launch m;;
