open Vecteur;;
open Boule2;;
open Barre2;;
open Niveau;;
open Graphique2;;

let launch niv = 
  let nb = ref niv.n in
(* nb représente le nombre de boules dans le niveau. *)
  let i = ref 0 in
(* i représente le nombre de boules supprimées pendant le tour *) 
draw_niveau niv;
while (vit_max niv > 30. && niv.n > 1) do
  reset_maybe niv.barre;
  quit_maybe niv.barre;
(* Si on a cliqué sur le bouton close, on ferme tout *)
  let j = !nb - niv.n in
(* On compte le nombre de boules supprimées au cours de la dernière évolution *)
  incr_score (niv.barre) (j*(!i+1 + !i+j)*50);
(* On incrémente le score du joueur en fonction : 100 points pour la première boule rentrée, 200 pour la suivant, etc... *)
  nb := niv.n; i := !i + j;
  evolution niv;
  Graphics.auto_synchronize false;
  draw_niveau niv;
  Graphics.auto_synchronize true;
done;
let j = !nb - niv.n in
incr_score (niv.barre) (j*(!i+1 + !i+j)*50);
(* Si on est sorti de la boucle parce que la dernière boule a été détruite, il faut quand même calculer le nouveau score! *)
Graphics.auto_synchronize false;
draw_niveau niv;
(* On redessine le niveau pour que le changement de score s'affiche. *)
Graphics.auto_synchronize true;;
(* Lance et affiche l'évolution du niveau, tant que la vitesse des boules reste raisonnable. *)

let random_boule r = 
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
{o={x=Random.float (xm -.r); y=Random.float(ym-.r)}; 
r=r; s=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}};;

let rec random_boule_liste n r = 
if n=0 then []
else (random_boule r) :: (random_boule_liste (n-1) r);;

let make_rangee_boule x n r =
  let ymin = -.r in
     let rec aux i l = 
       if i = 0 then l 
       else aux (i-1) ({o={x=x; y=float_of_int(i)*.2.*.r +. ymin}; r=r; s=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}} :: l) in
aux (n) [];;

let make_triangle_boule n r =
  let xm = float_of_int (Graphics.size_x()) in
  let ym = float_of_int (Graphics.size_y()) in
  let rec aux i l = 
    if i=0 then (make_rangee_boule (xm -. 5.*.r) n r) @ l
    else aux (i-1) ((make_rangee_boule (xm -. 5.*.r -. (float_of_int i)*.2.*.r) (n-i) r) @ l) in
{o={x=5.*.r; y=ym/.2.}; r=r*.0.8; s=infinity; v={x=0.; y=0.}; a={x=0.; y=0.}}::(aux n []);;

let make_niveau l =
if l = [] then failwith "Ce serait mieux d'avoir des boules à placer dans le niveau..."
else begin
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()-40) in
  let close = {o={x=xm-.20.;y=ym+.20.}; r=8.; s=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let reset = {o={x=xm-.50.;y=ym+.20.}; r=8.; s=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let barre = {score = 0; reset = reset; close = close} in
  let m = Array.of_list l in
  let n = Array.length m in
  let niv = {boules = m; n=n; barre=barre} in
draw_niveau niv;
niv
end;;

let initialise () = 
  let l = make_triangle_boule 8 20. in
make_niveau l;;
