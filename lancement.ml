open Vecteur;;
open Boule;;
open Trou;;
open Billard;;
open Graphique;;

let launch bill = 
draw_billard bill;
while (vit_max bill > 30. && bill.n > 0) do
  evolution bill;  
  Graphics.auto_synchronize false;
  draw_billard bill;
  Graphics.auto_synchronize true;
done;;
(* Lance et affiche l'évolution du billard, tant que la vitesse des boules reste raisonnable. *)


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
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
  let trou1 = {o={x=0.;y=0.}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou2 = {o={x=xm;y=0.}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou3 = {o={x=0.;y=ym}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou4 = {o={x=xm;y=ym}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou5 = {o={x=xm/.2.;y=0.}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}}
  and trou6 = {o={x=xm/.2.;y=ym}; r=r*.3.3; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}} in
  let trous = [|trou1; trou2; trou3; trou4; trou5; trou6|] in
  let m = Array.of_list l in
  let n = Array.length m in
  let bill = {boules = m; n=n; f = 0.995; trous=trous} in
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
