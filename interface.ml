open Graphics;;
open Vecteur;;
open Boule;;
open Billard;;
open Lancement;;
open Graphique;;

exception EmptySelection;;

let intersect_boule (x,y) b = carre (b.o.x -. (float_of_int x)) +. carre (b.o.y -. (float_of_int y)) < carre b.r ;;

let select_boule bill = 
  (*let n = bill.n in*)
  let t = bill.boules in
  let s = Graphics.wait_next_event [Graphics.Button_down] in
  let i = ref 0 in
(*
while (!i < n && not (intersect_boule (s.mouse_x, s.mouse_y) t.(!i))) do incr i done;
if !i = n then raise EmptySelection*)
if not (intersect_boule (s.mouse_x, s.mouse_y) t.(0)) then raise EmptySelection
else t.(!i);;

let rec select_real_boule bill = 
try select_boule bill
with EmptySelection -> select_real_boule bill;;

let lance_boule bill =
let b = select_real_boule bill in
let x,y = Graphics.mouse_pos() in
let s = Graphics.wait_next_event [Graphics.Button_down] in
let x2 = s.mouse_x and y2 = s.mouse_y in
b.v <- {x = 5. *. float_of_int (x-x2); y = 5. *. float_of_int (y-y2)};;

let rec place_boule bill = 
  let t  = ref (Sys.time()) in
  let b = bill.boules.(0) in
while not (Graphics.button_down()) do
  let x,y = Graphics.mouse_pos() in
  if (Sys.time() -. !t) > 0.01 then begin
  b.o.x <- (float_of_int x); b.o.y <- (float_of_int y);
  Graphics.auto_synchronize false;
  draw_billard bill;
  Graphics.auto_synchronize true; 
  t := Sys.time() end
done;
  let i = ref 1 in
while ((!i < bill.n) && (not (contact b bill.boules.(!i)))) do incr i done;
if !i < bill.n then place_boule bill;;

let partie bill =
  let b = copy_boule bill.boules.(0) in
while bill.n > 0 do
lance_boule bill;
if launch bill then (insert_boule b bill; place_boule bill) done;;
