open Graphics;;
open Vecteur;;
open Boule;;
open Billard;;
open Lancement;;

exception EmptySelection;;

let intersect_boule (x,y) b = carre (b.o.x -. (float_of_int x)) +. carre (b.o.y -. (float_of_int y)) < carre b.r ;;

let select_boule bill = 
  let n = bill.n in
  let t = bill.boules in
  let s = Graphics.wait_next_event [Graphics.Button_down] in
  let i = ref 0 in
while (!i < n && not (intersect_boule (s.mouse_x, s.mouse_y) t.(!i))) do incr i done;
if !i = n then raise EmptySelection
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

let partie bill =
while bill.n > 0 do
lance_boule bill;
launch bill done;;