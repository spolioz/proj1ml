open Vecteur;;
open Boule;;
open Trou;;
open Barre;;
open Billard;;
open Graphique;;

let launch bill = 
  let rep = ref false in
  let nb = ref bill.n in
(* nb représente le nombre de boules dans le billard. *)
  let i = ref 0 in
(* i représente le nombre de boules supprimées pendant le tour *) 
draw_billard bill;
while (not !rep && vit_max bill > 30. && bill.n > 1) do
  reset_maybe bill.barre;
  quit_maybe bill.barre;
(* Si on a cliqué sur le bouton close, on ferme tout *)
  let j = !nb - bill.n in
(* On compte le nombre de boules supprimées au cours de la dernière évolution *)
  incr_score (bill.barre) (j*(!i+1 + !i+j)*50);
(* On incrémente le score du joueur en fonction : 100 points pour la première boule rentrée, 200 pour la suivant, etc... *)
  nb := bill.n; i := !i + j;
  rep :=  evolution bill;
  Graphics.auto_synchronize false;
  draw_billard bill;
  Graphics.auto_synchronize true;
done;
let j = !nb - bill.n in
if not !rep then
incr_score (bill.barre) (j*(!i+1 + !i+j)*50);
(* Si on est sorti de la boucle parce que la dernière boule a été rentrée, il faut quand même calculer le nouveau score! *)
next_turn bill.barre;
Graphics.auto_synchronize false;
draw_billard bill;
Graphics.auto_synchronize true;
(* On redessine le billard pour que le changement de tour apparaisse effectivement au niveau des couleurs d'affichage des joueurs. *)
!rep;;
(* Lance et affiche l'évolution du billard, tant que la vitesse des boules reste raisonnable. 
Renvoie true si la boule d'indice 0 a été supprimée au cours de ce lancer. *)


(* Fonctions de création aléatoire utiles *)

let random_boule r = 
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
{o={x=Random.float (xm -.r); y=Random.float(ym-.r)}; 
r=r; m=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}};;

let rec random_boule_liste n r = 
if n=0 then []
else (random_boule r) :: (random_boule_liste (n-1) r);;

let make_billard l =
if l = [] then failwith "Ce serait mieux d'avoir des boules à placer dans le billard..."
else begin
  let r = (List.hd l).r in
  let r1 = r*.2. in
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()-40) in
  let trou1 = {o={x=r1;y=r1}; r=r1; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou2 = {o={x=xm-.r1;y=r1}; r=r1; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou3 = {o={x=r1;y=ym-.r1}; r=r1; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou4 = {o={x=xm-.r1;y=ym-.r1}; r=r1; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou5 = {o={x=xm/.2.;y=r1}; r=r1; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou6 = {o={x=xm/.2.;y=ym-.r1}; r=r1; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let trous = [|trou1; trou2; trou3; trou4; trou5; trou6|] in
  let close = {o={x=xm-.20.;y=ym+.20.}; r=8.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let reset = {o={x=xm-.50.;y=ym+.20.}; r=8.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let barre = {j1 = (true,0); j2 = (false,0); reset = reset; close = close} in
  let m = Array.of_list l in
  let n = Array.length m in
  let bill = {boules = m; n=n; f = 0.995; trous=trous; barre=barre} in
(*
  let l = ref [] in
  for i = 1 to n-1 do for j=0 to i-1 do
    if contact m.(i) m.(j) then l:= (i,j) :: (!l)
  done done;
  while !l <> [] do
    List.iter (fun (i,j) -> separe m.(i) m.(j)) (!l);
    l:=[];
    for i = 1 to n-1 do for j=0 to i-1 do
      if contact m.(i) m.(j) then l:= (i,j) :: !l 
    done done;
  done;
*)
draw_billard bill;
bill
end;;

let make_rangee_boule x n r =
  let ym = float_of_int (Graphics.size_y()) in
  let n' = float_of_int n in
if n' *. r > ym then failwith "Le billard est trop petit pour accueillir autant de boules!"
else let ymin = (ym/.2.) -. ((n'+.1.)*.r) in
     let rec aux i l = 
       if i = 0 then l 
       else aux (i-1) ({o={x=x; y=float_of_int(i)*.2.*.r +. ymin}; r=r; m=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}} :: l) in
aux (n) [];;

let make_triangle_boule n r =
  let xm = float_of_int (Graphics.size_x()) in
  let ym = float_of_int (Graphics.size_y()) in
  let rec aux i l = 
    if i=0 then (make_rangee_boule (xm -. 5.*.r) n r) @ l
    else aux (i-1) ((make_rangee_boule (xm -. 5.*.r -. (float_of_int i)*.(sqrt 3.)*.r) (n-i) r) @ l) in
{o={x=5.*.r; y=ym/.2.}; r=r*.0.8; m=1000.; v={x=0.; y=0.}; a={x=0.; y=0.}}::(aux n []);;

let initialise() = let l = make_triangle_boule 5 20. in
make_billard l;;
(* Initialise un billard prêt pour une nouvelle partie. *)
