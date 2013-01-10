open Graphics;;

type bouton = {xmin : int; xmax : int; ymin : int; ymax : int; s : string};;
(* Un bouton est un cadre repéré par ses coordonnées extrémales, avec une chaîne de caractère décrivant son action. ("OUI", "NON", etc...)*)

let select_bouton bout =
  let (x,y) = Graphics.mouse_pos() in
 Graphics.button_down() && x >= bout.xmin && x <= bout.xmax && y >= bout.ymin && y <= bout.ymax;;
(* Renvoie le booléen indiquant si l'utilisateur a cliqué sur le bouton bout. *)
