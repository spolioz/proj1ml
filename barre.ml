open Graphics;;
open Vecteur;;
open Boule;;


let intersect_boule (x,y) b = carre (b.o.x -. (float_of_int x)) +. carre (b.o.y -. (float_of_int y)) < carre b.r ;;
(* Indique si la position (x,y) est recouverte par la boule b. *)

type barre = {mutable j1 : bool * int; mutable j2 : bool * int; reset : boule; close : boule};;
(* j1 et j2 contiennent les informations concernant les joueurs 1 et 2. Le booléen indique si le tour concerne le joueur concerné, l'entier indique le score du joueur. close est un bouton permettant de fermer le billard. *)

exception Close;;
let quit_maybe barre = if (Graphics.button_down() && intersect_boule (Graphics.mouse_pos()) barre.close) then raise Close
  else ();;
(* Renvoie l'exception Close quand l'utilisateur appuie sur le bouton close. *)

exception Reset;;
let reset_maybe barre = if (Graphics.button_down() && intersect_boule (Graphics.mouse_pos()) barre.reset) then raise Reset
  else ();;

let next_turn barre = let (a,x) = barre.j1 and (b,y) = barre.j2 in
barre.j1 <- (not a, x); barre.j2 <- (not b, y);;
(* Changement de tour : c'est à l'autre joueur de jouer. *)

let incr_score barre n = let (a,x) = barre.j1 and (b,y) = barre.j2 in
if a then barre.j1 <- (a,x+n) else barre.j2 <- (b,y+n);;
