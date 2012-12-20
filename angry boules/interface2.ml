open Graphics;;
open Vecteur;;
open Boule2;;
open Niveau;;
open Lancement2;;
open Graphique2;;
open Barre2;;

exception EmptySelection;;

let rec select_boule niv = 
  (*let n = bill.n in*)
  reset_maybe niv.barre;
  quit_maybe niv.barre;
  let t = niv.boules in
  let s = Graphics.wait_next_event [Graphics.Button_down] in
  let i = ref 0 in
if not (intersect_boule (s.mouse_x, s.mouse_y) t.(0)) then select_boule niv
else t.(!i);;

let lance_boule niv =
reset_maybe niv.barre;
quit_maybe niv.barre;
let b = select_boule niv in
let x,y = Graphics.mouse_pos() in
let s = Graphics.wait_next_event [Graphics.Button_down] in
let x2 = s.mouse_x and y2 = s.mouse_y in
b.v <- {x = 5. *. float_of_int (x2-x); y = 5. *. float_of_int (y2-y)};;

(*
let lance_boule bill =
let t  = ref (Sys.time()) in
let b = select_real_boule bill in
let x,y = Graphics.mouse_pos() in
while not (Graphics.button_down()) do
  let x1,y1 = Graphics.mouse_pos() in
  if (Sys.time() -. !t) > 0.01 then begin
    Graphics.auto_synchronize false;
    draw_billard bill;
    Graphics.set_color Graphics.black;
    Graphics.draw_segments [|x,y,x1,y1|];
    Graphics.auto_synchronize true;
    t := Sys.time()
  end
done;
let x2,y2 = Graphics.mouse_pos() in
b.v <- {x = 5. *. float_of_int (x2-x); y = 5. *. float_of_int (y2-y)};;
*)


let rec partie n =
  let ym = float_of_int (Graphics.size_y()) in
  let niv = initialise() in
  let i = ref n in
(* n sera le nombre de coups autorisés pour tenter de réussir le niveau *)
try (
try (
  while (niv.n > 1 && !i > 0) do
    niv.boules.(0).o.x <- 5. *. niv.boules.(0).r;
    niv.boules.(0).o.y <- ym /. 2.;
    draw_niveau niv;
    quit_maybe niv.barre;
    lance_boule niv;
    launch niv;
    decr i
  done;
  draw_result niv; partie n
)
with Close -> Graphics.close_graph()
)
with Reset -> partie n;;