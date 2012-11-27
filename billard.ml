#load "graphics.cma";;
Graphics.open_graph "";;

let f1 = 0.995 ;;
(* On multiplie la vitesse par f1 après chaque instant dt, ce qui traduit les frottements. *)

let dt = 0.01 ;;
(* dt sera la frame choisie pour l'affichage séquentiel. *)

type vect = {mutable x : float; mutable y : float};;
(*un vecteur permettra de définir un point, un vitesse, une accélération, ...*)

type boule = {mutable o : vect; r : float; m : float; mutable v : vect; mutable a : vect;};;
(* On repère une boule par son centre o, son rayon r, sa masse m, sa vitesse v, et son accélération a. *)

let b1 = {o={x=200.;y=60.}; r=20.; m=8000.; v ={x=Random.float 3000. ;y=Random.float 3000.}; a = {x=0.; y=0.}};;
let b2 = {o={x=180.;y=180.}; r=20.; m=125000.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let b3 = {o={x=300.;y=300.}; r=20.; m=125000.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
type billard = {boules : boule array; mutable n : int; f : float; trous : boule array};;
let trou1 = {o={x=0.;y=0.}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let trou2 = {o={x=float_of_int (Graphics.size_x());y=0.}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let trou3 = {o={x=float_of_int (Graphics.size_x());y=float_of_int (Graphics.size_y())}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let trou4 = {o={x=0.;y=float_of_int (Graphics.size_y())}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let m = {boules = [|b1;b2;b3|]; n=3; f=0.999; trous = [|trou1; trou2; trou3; trou4|]};;
(* L'exemple sur lequel on testera nos fonctions *)

let copy_boule b = {o = {x = b.o.x; y = b.o.y}; r = b.r; m = b.m; v = {x = b.v.x; y = b.v.y}; a = {x = b.a.x; y = b.a.y};};;
let copy_billard bill = 
  let m = bill.boules in
  let n = bill.n in
  let b = copy_boule m.(0) in
  let rep = Array.create n b in
  for i = 1 to n-1 do rep.(i) <- copy_boule m.(i) done;
{boules = rep; n = n; f = bill.f; trous = bill.trous};;
(* Au cas où on voudrait avoir une copie indépendante d'un billard. *)

let swap_tab i j tab = let x = tab.(j) in tab.(j) <- tab.(i); tab.(i) <- x;;
let supprime_boule i bill = let n = bill.n in
swap_tab i (n-1) (bill.boules); bill.n <- n-1;;

let carre x = x *. x;;
let distance b1 b2 = sqrt (carre (b1.o.x -. b2.o.x) +. carre (b1.o.y -. b2.o.y));;
(* Donne la distance entre les centres de deux boules. *)
let vitesse b1 = sqrt (carre b1.v.x +. carre b1.v.y);;
(* Donne la norme de la vitesse d'une boule. *)

let scalaire u v = u.x*.v.x +. u.y*.v.y;;
(* Implémentation du produit scalaire sur deux vecteurs. *)
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
b.v.x <- b.v.x *. f1 +. (dt *. b.a.x);
b.v.y <- b.v.y *. f1 +. (dt *. b.a.y);
b.o.x <- b.o.x +. (dt *. b.v.x);
b.o.y <- b.o.y +. (dt *. b.v.y);
b.a.x <- 0.;
b.a.y <- 0.;;
(* Fait avancer les boules durant l'intervalle de temps dt, 
selon leur vitesse, sans prendre en compte les collisions. *)

let contact b1 b2 = distance b1 b2 <= (b1.r +. b2.r);;
(* Détermine si oui ou non deux boules sont en contact strict. *)

let contact_triple b1 b2 b3 = contact b1 b2 && contact b2 b3 && contact b1 b3;;
(* Détermine si trois boules sont toutes en contact entre elles. *)

let separe b1 b2 = 
  let bary = mult_scalaire (1./.(b1.r +. b2.r)) (add_vect (mult_scalaire b2.r b1.o) (mult_scalaire b1.r b2.o)) in
  let u = vect_dir b1 b2 in
b1.o <- add_vect bary (mult_scalaire (-. b1.r) u);
b2.o <- add_vect bary (mult_scalaire (b2.r) u);;

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

let contact_trou b trou = distance b trou < trou.r;;
(* Indique si oui ou non une boule est en contact avec un trou. *)

let tombe_trou b trou = distance b trou < (trou.r -. b.r);;
(* Indique si oui ou non une boule est en position pour tomber dans un trou. *)

let interaction_trou b trou = 
  let u = vect_dir b trou in
  let d = (trou.r -. (distance b trou))/.trou.r in
  let a = 50000.*. d *. sqrt (1. -. (carre d)) in
b.a <- mult_scalaire a u;;
(* Fait subir l'attraction d'un trou à une boule. *)

type direction = Droite | Gauche | Haut | Bas | Nil;;

let out_of_billard b = 
  let xm = float_of_int (Graphics.size_x()) 
  and ym = float_of_int (Graphics.size_y()) in
if b.o.x -. b.r < 0. then Gauche
else if b.o.x +. b.r > xm then Droite
else if b.o.y -. b.r < 0. then Bas
else if b.o.y +. b.r > ym then Haut
else Nil;;
(* Renvoie Nil si la boule est à l'intérieur strictement du billard,
et sinon la direction dans laquelle elle commence à sortir du billard. *)

let ricochet b dir =
  let xm = float_of_int (Graphics.size_x()) 
  and ym = float_of_int (Graphics.size_y()) in
match dir with
  |Droite -> b.v.x <- -. b.v.x; b.o.x <- xm -. b.r
  |Gauche -> b.v.x <- -. b.v.x; b.o.x <- b.r
  |Haut -> b.v.y <- -. b.v.y; b.o.y <- ym -. b.r
  |Bas -> b.v.y <- -. b.v.y; b.o.y <- b.r
  |Nil -> ();;
(* Gère le rebond sur un bord de la fenêtre graphique (= du billard), 
en fonction de la direction d'échappement. *)

let evolution bill =
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
for i = 1 to n-1 do for j = 0 to i-1 do
  if contact m.(i) m.(j) 
  then begin
    let test_contact = ref false in
    for k = j+1 to i-1 do
      if contact_triple m.(i) m.(j) m.(k)
      then
(	collision_triple m.(i) m.(j) m.(k) ; test_contact := true )
    done; 
    if not !test_contact then collision m.(i) m.(j) end
  (* On fait rebondir les boules qui s'entrechoquent. *)
done done;
let n1 = Array.length bill.trous in
let i = ref 0 in
while !i < bill.n do
(* Au fur et à mesure que l'on supprime des boules, la valeur de bill.n va
être modifiée : c'est pour cela que l'on utilise une boucle while, qui teste
donc à chaque itération la nouvelle valeur de bill.n. *)
  for j = 0 to n1-1 do
    if tombe_trou m.(!i) bill.trous.(j) 
    then (supprime_boule !i bill)
    (* On supprime les boules qui sont tombées dans un trou. *)
    else if contact_trou m.(!i) bill.trous.(j) 
    then interaction_trou m.(!i) bill.trous.(j)
    (* On fait subir l'attraction du trou aux boules qui le touchent. *)
  done;
  incr i
done;
while (Sys.time() -. t < dt) do () done
(* On attend dt, afin que l'affichage séquentiel suive la frame. *)
;;
(* Fait passer la configuration d'un billard de l'instant t à l'instant t+dt. *)

let vit_max bill = let n = bill.n in
		   let m = bill.boules in
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
let draw_trou b = Graphics.set_color Graphics.black; 
Graphics.fill_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
let clear_boule b = Graphics.set_color Graphics.white; 
Graphics.fill_circle(int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
(* Pour effacer une boule de la fenêtre graphique. *)
let draw_billard bill = 
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
  let m = bill.boules in
  let n = bill.n in
reset();
Graphics.set_color (Graphics.rgb 13 191 49);
Graphics.fill_rect 0 0 xm ym;
let n1 = Array.length bill.trous in
for i = 0 to n1-1 do 
  draw_trou bill.trous.(i) done;
for i = 0 to n-1 do
  draw_boule m.(i) done
;;
(* Pour dessiner la configuration actuelle d'un billard dans la fenêtre graphique. *)

let launch bill = 
draw_billard bill;
while (vit_max bill > 30. && bill.n > 0) do
  evolution bill;  
  Graphics.auto_synchronize false;
  draw_billard bill;
  Graphics.auto_synchronize true;
done;;
(* Lance et affiche l'évolution du billard, tant que la vitesse des boules reste raisonnable. *)


(* Fonctions de création aléatoire utiles *)

let random_boule r = 
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
{o={x=Random.float (xm -.r); y=Random.float(ym-.r)}; 
r=r; m=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}};;

let rec random_boule_liste n r = 
if n=0 then []
else (random_boule r) :: (random_boule_liste (n-1) r);;

let make_billard l =
if l = [] then failwith "Ce serait mieux d'avoir des boules à placer dans le billard..."
else begin
  let r = (List.hd l).r in
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
  let trou1 = {o={x=0.;y=0.}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou2 = {o={x=xm;y=0.}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou3 = {o={x=0.;y=ym}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou4 = {o={x=xm;y=ym}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou5 = {o={x=xm/.2.;y=0.}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou6 = {o={x=xm/.2.;y=ym}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let trous = [|trou1; trou2; trou3; trou4; trou5; trou6|] in
  let m = Array.of_list l in
  let n = Array.length m in
  let bill = {boules = m; n=n; f = 0.995; trous=trous} in
(*
  let l = ref [] in
  for i = 1 to n-1 do for j=0 to i-1 do
    if contact m.(i) m.(j) then l:= (i,j) :: (!l)
  done done;
  while !l <> [] do
    List.iter (fun (i,j) -> separe m.(i) m.(j)) (!l);
    l:=[];
    for i = 1 to n-1 do for j=0 to i-1 do
      if contact m.(i) m.(j) then l:= (i,j) :: !l 
    done done;
  done;
*)
draw_billard bill;
bill
end;;

let random_color() = Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255);;

let make_rangee_boule x n r =
  let ym = float_of_int (Graphics.size_y()) in
  let n' = float_of_int n in
if n' *. r > ym then failwith "Le billard est trop petit pour accueillir autant de boules!"
else let ymin = (ym/.2.) -. ((n'+.1.)*.r) in
     let rec aux i l = 
       if i = 0 then l 
       else aux (i-1) ({o={x=x; y=float_of_int(i)*.2.*.r +. ymin}; r=r; m=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}} :: l) in
aux (n) [];;

let make_triangle_boule n r =
  let xm = float_of_int (Graphics.size_x()) in
  let ym = float_of_int (Graphics.size_y()) in
  let rec aux i l = 
    if i=0 then (make_rangee_boule (xm -. 5.*.r) n r) @ l
    else aux (i-1) ((make_rangee_boule (xm -. 5.*.r -. (float_of_int i)*.(sqrt 3.)*.r) (n-i) r) @ l) in
{o={x=5.*.r; y=ym/.2.}; r=r*.0.8; m=1000.; v={x=0.; y=0.}; a={x=0.; y=0.}}::(aux n []);;

let l = make_triangle_boule 6 20.;;

let m = make_billard l;;

(*while m.n > 0 do*)
m.boules.(21).o.x <- 120.;
m.boules.(21).o.y <- (float_of_int (Graphics.size_y())) /. 2.;;
draw_billard m;;

m.boules.(21).v.x <- 500.;
m.boules.(21).v.y <- 0.;
launch m (*done*);;


type surface = float * float * float * float;;
(* Une surface est un rectangle repéré par ses 
abscisses et ordonnées minimales et maximales. *)

type quadtree = F of surface*(boule list) | N of quadtree*quadtree*quadtree*quadtree;;
(* Un quadtree renseigne ses subdivision par structure arborescente, ainsi que la liste
des boules intersectant chaque subdivision. *)

let rec rayon_max = function
  | [] -> 0.
  | b :: r -> max b.r (rayon_max r);;
(* Calcule le rayon maximal d'une liste de boules. *)

let rec boule_list_intersect xmin xmax ymin ymax = function
  | [] -> []
  | b :: r -> if (b.o.x +. b.r > xmin && b.o.x -. b.r < xmax && b.o.y +. b.r > ymin && b.o.y -. b.r < ymax)
    then b :: (boule_list_intersect xmin xmax ymin ymax r)
    else boule_list_intersect xmin xmax ymin ymax r;;

let rec quadtree_create l =
  let r = rayon_max l in
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
  let n = int_of_float (max (xm/.(4.*.r)) (ym/.(4.*.r))) in
  let rec aux n xmin xmax ymin ymax l = 
    if n = 0 then F((xmin, ymin, xmax, ymax), (boule_list_intersect xmin xmax ymin ymax l))
    else let x1 = (xmin +. xmax)/.2. and y1 = (ymin +. ymax)/.2. in
	 N(aux (n-1) x1 xmax ymin y1 (boule_list_intersect x1 xmax ymin y1 l), 
	   aux (n-1) x1 xmax y1 ymax (boule_list_intersect x1 xmax y1 ymax l), 
	   aux (n-1) xmin x1 ymin y1 (boule_list_intersect xmin x1 ymin y1 l), 
	   aux (n-1) xmin x1 y1 ymax (boule_list_intersect xmin x1 y1 ymax l)) in
aux n 0. xm 0. ym;;
