type vect = {mutable x : float; mutable y : float};;
(*un vecteur permettra de définir un point, un vitesse, une accélération, ...*)

let scalaire u v = u.x*.v.x +. u.y*.v.y;;
(* Implémentation du produit scalaire sur deux vecteurs. *)
let add_vect u v = {x = u.x +. v.x; y = u.y +. v.y};;
(* addition sur les vecteurs. *)
let sous_vect u v = {x = u.x -. v.x; y = u.y -. v.y};;
(* Soustraction sur les vecteurs. *)
let mult_scalaire k v =  {x = k *. v.x; y = k *. v.y};;
(* Multiplication d'un vecteur par un scalaire flottant. *)

let carre x = x *. x;;
let norme v = sqrt (carre v.x +. carre v.y);;
