open Vecteur;;
open Boule2;;
open Graphics;;
open Barre2;;
open Obstacle;;

Graphics.open_graph " 1200x800";;

type niveau = {boules : boule2 array; mutable n : int; mutable obs : obstacle list; barre : barre};;
(* Un niveau contient des boules répertoriées dans un tableau. n est le nombre de boules restantes dans le niveau (on ne considère alors que les n premières boules du tableau). 
obs est la liste d'obstacles (murs) dans le niveau, et enfin la barre permet à l'utilisateur des actions de base (fermer, relancer un niveau, voir son score). *)

let swap_tab i j tab = let x = tab.(j) in tab.(j) <- tab.(i); tab.(i) <- x;;
(* Echange les éléments d'indices i et j dans le tableau tab. *)

let supprime_boule i niv = let n = niv.n in
swap_tab i (n-1) (niv.boules); niv.n <- n-1;;
(* Supprime la boule d'indice i dans le niveau. *)

let collision i j niv =
  let b1 = niv.boules.(i) and b2 = niv.boules.(j) in 
separe b1 b2;
  let u = vect_dir b1 b2 in
  let dv = mult_scalaire (scalaire (sous_vect b1.v b2.v) u) u in
  let ds = (norme dv)*.4. in
if ds > b1.s then begin
  let dv' = mult_scalaire (abs_float (b1.s /. ds)) dv in
(* La boule détruite dévie d'autant moins la boule projectile que cette première était fragilisée. On a b1.s/ds < 1, et donc dv' < dv *)
  b2.v <- add_vect b2.v dv';
  supprime_boule i niv end
else if ds > b2.s then begin
  let dv' = mult_scalaire (b2.s /. ds) dv in
  b1.v <- sous_vect b1.v dv';
  supprime_boule j niv end
else begin
  b2.v <- mult_scalaire 0.93 (add_vect b2.v dv);
  b1.v <- mult_scalaire 0.93 (sous_vect b1.v dv);
  reaction_support b1 b2 end;
if ds > 800. then begin
(* Si l'impact est assez fort, on diminue la solidité de chancune des deux boules. *)
  b2.s <- b2.s -. ds;
  b1.s <- b1.s -. ds
end;;
(* Gère la collision entre deux boules (uniquement si elles sont en contact!) *)

let exists_contact b niv = let i = ref 0 in 
while (!i < niv.n && not (contact b niv.boules.(!i))) do incr i done;
!i < niv.n;;
(* Détermine si la boule b est en contact avec une autre dans le niveau. *)

type direction = Droite | Gauche | Haut | Bas | Nil;;

let out_of_niv b =
  let xm = float_of_int (Graphics.size_x()) 
  and ym = float_of_int (Graphics.size_y() - 40) in
if b.o.y -. b.r < 0. then Bas
(* On teste le bas en premier, car c'est le plus important :
c'est le seul qui entraîne un rebond dans le niveau! *)
else if b.o.x +. b.r < 0. then Gauche
else if b.o.x -. b.r > xm then Droite
else if b.o.y +. b.r > ym then Haut
else Nil;;
(* Renvoie Nil si la boule est à l'intérieur strictement du niveau,
et sinon la direction dans laquelle elle commence à sortir du niveau. *)

let ricochet b dir = match dir with
  |Bas -> b.v.y <- (-.0.4) *. b.v.y; b.o.y <- b.r;
    let ds = 10. *. (abs_float b.v.y) in if ds > 2000. then  b.s <- b.s -. ds;
    b.a.y <- 0.
  |_ -> ();;
(* Gère le rebond sur le sol. *)

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
  List.iter (collision_obstacle m.(i)) niv.obs;
  let dir = out_of_niv m.(i) in 
  ricochet m.(i) dir
  (* La boule tombe dans le sol, il faut donc la faire rebondir. *)
done;
let i = ref 1 in
while !i < niv.n do
  let j = ref 0 in
  while !j < !i do
    if contact m.(!i) m.(!j) 
    then collision !i !j niv;
    incr j done;
  incr i done;
  (* On fait rebondir les boules qui s'entrechoquent. *)
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
(* Fait passer la configuration d'un niveau de l'instant t à l'instant t+dt. *)

let vit_max niv = let n = niv.n in
		   let m = niv.boules in
let max = ref (vitesse m.(1)) in
for i = 2 to n-1 do
  let v = vitesse m.(i) in
  if v > !max then max := v
done;
let v = vitesse m.(0) in
if (out_of_niv m.(0) <> Gauche  && out_of_niv m.(0) <> Droite) && (v > !max)
then v
else !max;;
(* Renvoie la vitesse de la boule la plus rapide à l'intérieur du niveau. 
Cas particulier de la boule projectile qui n'est jamais supprimée... *)
