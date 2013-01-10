open Graphics;;
open Vecteur;;
open Boule2;;


let intersect_boule (x,y) b = carre (b.o.x -. (float_of_int x)) +. carre (b.o.y -. (float_of_int y)) < carre b.r ;;
(* Indique si la position (x,y) est recouverte par la boule b. *)

type barre = {mutable score : int; reset : boule2; close : boule2};;
(* Contient l'indication du score en cours, et les deux boutons permettant de relancer le niveau, ou de fermer le jeu, considérés comme des boules fixes. *)

exception Close;;
let quit_maybe barre = if (Graphics.button_down() && intersect_boule (Graphics.mouse_pos()) barre.close) then raise Close
  else ();;
(* Renvoie l'exception Close quand l'utilisateur appuie sur le bouton close. *)

exception Reset;;
let reset_maybe barre = if (Graphics.button_down() && intersect_boule (Graphics.mouse_pos()) barre.reset) then raise Reset
  else ();;
(* Même chose avec Reset. *)

let incr_score barre n = barre.score <- barre.score + n;;
(* Pour incrémenter le score de n dans la barre. *)
