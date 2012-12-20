open Graphics;;
open Vecteur;;
open Boule2;;


let intersect_boule (x,y) b = carre (b.o.x -. (float_of_int x)) +. carre (b.o.y -. (float_of_int y)) < carre b.r ;;
(* Indique si la position (x,y) est recouverte par la boule b. *)

type barre = {mutable score : int; reset : boule2; close : boule2};;
(* j1 et j2 contiennent les informations concernant les joueurs 1 et 2. Le booléen indique si le tour concerne le joueur concerné, l'entier indique le score du joueur. close est un bouton permettant de fermer le billard. *)

exception Close;;
let quit_maybe barre = if (Graphics.button_down() && intersect_boule (Graphics.mouse_pos()) barre.close) then raise Close
  else ();;
(* Renvoie l'exception Close quand l'utilisateur appuie sur le bouton close. *)

exception Reset;;
let reset_maybe barre = if (Graphics.button_down() && intersect_boule (Graphics.mouse_pos()) barre.reset) then raise Reset
  else ();;

let incr_score barre n = barre.score <- barre.score + n;;
