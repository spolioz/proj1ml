open Graphics;;
open Vecteur;;
open Boule2;;
open Niveau;;
open Lancement2;;
open Graphique2;;
open Barre2;;


let rec select_boule niv = 
  (*let n = bill.n in*)
  reset_maybe niv.barre;
  quit_maybe niv.barre;
  let t = niv.boules in
  let s = Graphics.wait_next_event [Graphics.Button_down] in
  let (x,y) = (s.mouse_x, s.mouse_y) in
  let i = ref 0 in
if not (intersect_boule (x, y) t.(0)) then select_boule niv
else t.(!i);;
(* Pour sélectionner une boule dans le niveau. En l'occurence, seule la boule
d'indice 0 (projectile) a le droit d'être sélectionnée. *)

let lance_boule niv =
reset_maybe niv.barre;
quit_maybe niv.barre;
let b = select_boule niv in
let x,y = Graphics.mouse_pos() in
let s = Graphics.wait_next_event [Graphics.Button_down] in
let x2 = s.mouse_x and y2 = s.mouse_y in
b.v <- {x = 5. *. float_of_int (x2-x); y = 5. *. float_of_int (y2-y)};;
(* Pour faire sélectionner puis lancer la boule projectile par l'utilisateur. *)
