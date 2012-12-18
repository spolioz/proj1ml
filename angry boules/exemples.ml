open Vecteur;;
open Boule2;;
open Barre2;;
open Bouton;;
open Graphique2;;
open Lancement2;;
open Niveau;;



let l = make_triangle_boule 8 20.;;

let m = make_niveau l;;

m.boules.(0).v.x <- 500.;;
m.boules.(0).v.y <- 500.;;

launch m;;
