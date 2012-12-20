open Vecteur;;
open Boule2;;
open Graphics;;
open Barre2;;

Graphics.open_graph " 1200x800";;

type niveau = {boules : boule2 array; mutable n : int; barre : barre};;

let swap_tab i j tab = let x = tab.(j) in tab.(j) <- tab.(i); tab.(i) <- x;;
let supprime_boule i niv = let n = niv.n in
swap_tab i (n-1) (niv.boules); niv.n <- n-1;;

let exists_contact b niv = let i = ref 0 in 
while (!i < niv.n && not (contact b niv.boules.(!i))) do incr i done;
!i < niv.n;;

type direction = Droite | Gauche | Haut | Bas | Nil;;

let out_of_niv b = 
if b.o.y -. b.r < 0. then Bas
else Nil;;
(* Renvoie Nil si la boule est à l'intérieur strictement du billard,
et sinon la direction dans laquelle elle commence à sortir du billard. *)

let ricochet b dir = match dir with
  |Bas -> b.v.y <- (-.0.4) *. b.v.y; b.o.y <- b.r;
    let ds = 10. *. (abs_float b.v.y) in if ds > 2000. then  b.s <- b.s -. ds;
    b.a.y <- 0.
  |_ -> ();;
(* Gère le rebond sur un bord de la fenêtre graphique (= du billard), 
en fonction de la direction d'échappement. *)

let evolution niv =
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y() - 40) in
niv.barre.close.o.x <- xm -. 20.;
niv.barre.close.o.y <- ym +. 20.;
niv.barre.reset.o.x <- xm -. 50.;
niv.barre.reset.o.y <- ym +. 20.;
  let m = niv.boules in
  let n = niv.n in
  let t = Sys.time() in
for i = 0 to n-1 do
  evolution_boule m.(i) niv
  (* On commence par faire avancer les boules selon leur vitesse. *)
done;
for i = 0 to n-1 do
  let dir = out_of_niv m.(i) in 
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
(* Au fur et à mesure que l'on supprime des boules, la valeur de niv.n va
être modifiée : c'est pour cela que l'on utilise une boucle while, qui teste
donc à chaque itération la nouvelle valeur de niv.n. *)
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
let max = ref (vitesse m.(1)) in
for i = 2 to n-1 do
  let v = vitesse m.(i) in 
  if v > !max then max := v
done;
let v = vitesse m.(0) in
if (out_of_niv m.(0) = Nil  || out_of_niv m.(0) = Haut) && (v > !max)
then v
else !max;;
(* Renvoie la vitesse de la boule la plus rapide à l'intérieur du niveau. 
Cas particulier de la boule blanche qui n'est jamais supprimée... *)
