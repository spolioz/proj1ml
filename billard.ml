open Vecteur;;
open Boule;;
open Trou;;
open Graphics;;

Graphics.open_graph "";;

type billard = {boules : boule array; mutable n : int; f : float; trous : boule array};;

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
(* Supprime la boule d'indice i dans le billard. *)

let insert_boule b bill = bill.n <- bill.n + 1;
bill.boules.(bill.n-1) <- bill.boules.(0);
bill.boules.(0) <- b;;
(* Insère la boule b dans le billard. Il ne faut pas dépasser le nombre de boule initial du billard!
Il est donc préférable de n'utiliser cette fonction qu'une fois qu'une boule a été supprimée.
En l'occurence, on l'utilisera pour réinsérer la boule blanche quand cette dernière tombera dans un trou. *)

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
(* Fait passer la configuration d'un billard de l'instant t à l'instant t+dt.
Renvoie true si la boule d'indice 0 a été supprimée, false sinon. *)

let vit_max bill = let n = bill.n in
		   let m = bill.boules in
let max = ref (vitesse m.(0)) in
for i = 1 to n-1 do
  let v = vitesse m.(i) in 
  if v > !max then max := v
done;
!max;;
(* Renvoie la vitesse de la boule la plus rapide à l'intérieur du billard. *)


