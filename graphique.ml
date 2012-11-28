open Vecteur;;
open Billard;;
open Boule;;
open Trou;;


let reset() = Graphics.set_color Graphics.black;
Graphics.clear_graph();
Graphics.moveto 0 0;;
(* Réinitialise le graphe. *)

let draw_boule b = Graphics.set_color Graphics.red; 
Graphics.fill_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);
Graphics.set_color Graphics.black;
Graphics.draw_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
(* Pour dessiner une boule dans la fenêtre graphique. *)
let draw_trou b = Graphics.set_color Graphics.black; 
Graphics.fill_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
let clear_boule b = Graphics.set_color Graphics.white; 
Graphics.fill_circle(int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
(* Pour effacer une boule de la fenêtre graphique. *)
let draw_billard bill = 
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
  let m = bill.boules in
  let n = bill.n in
reset();
Graphics.set_color (Graphics.rgb 13 191 49);
Graphics.fill_rect 0 0 xm ym;
let n1 = Array.length bill.trous in
for i = 0 to n1-1 do 
  draw_trou bill.trous.(i) done;
for i = 0 to n-1 do
  draw_boule m.(i) done
;;
(* Pour dessiner la configuration actuelle d'un billard dans la fenêtre graphique. *)

let random_color() = Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255);;

let traduce_color n = 
  let b = n mod 256 in
  let n' = (n - b) / 256 in
  let g = n' mod 256 in
  let r = (n' - g) / 256 in
  (r, g, b);;
(* Renvoie la décomposition d'une couleur en rouge, vert et bleu. *)

let degrade xmin xmax ymin ymax color1 color2 = 
  let (r1, g1, b1) = traduce_color color1 
  and (r2, g2, b2) = traduce_color color2 in
  let dr = r2 - r1 and dg = g2 - g1 and db = b2 - b1 in
  let dx = xmax - xmin and dy = ymax - ymin in
for i = 0 to dy do
Graphics.set_color (Graphics.rgb (r1+(i*dr)/dy) (g1+(i*dg)/dy) (b1+(i*db)/dy));
Graphics.fill_rect xmin (ymin+i) dx 1
done;;
(* Fait un dégradé, parce que c'est plutôt classe! *)
