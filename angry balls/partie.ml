open Graphics;;
open Vecteur;;
open Boule2;;
open Niveau;;
open Lancement2;;
open Graphique2;;
open Barre2;;
open Interface2;;

let rec partie n =
  let niv = make_niveau (make_colonnes_boule 0 30.) in
(* On initialise avec un niveau vide pour afficher le fond de base. *)
try
  let q = draw_reset_choice niv.barre in
  let ym = float_of_int (Graphics.size_y()) in
  let niv = initialise q in
  let i = ref n in
(* n sera le nombre de coups autorisés pour tenter de réussir le niveau *)
  while (niv.n > 1 && !i > 0) do
    niv.boules.(0).o.x <- 5. *. niv.boules.(0).r;
    niv.boules.(0).o.y <- ym /. 2.;
    draw_niveau niv;
    quit_maybe niv.barre;
    lance_boule niv;
    launch niv;
    decr i
  done;
  draw_result niv
with 
  | Reset -> partie n
  | Close -> Graphics.close_graph();;
(* Lance une partie : fait choisir à l'utilisateur le niveau qu'il souhaite, et lui accorde n coups pour le réussir. *)

partie 3;;
