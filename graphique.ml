open Vecteur;;
open Billard;;
open Boule;;
open Trou;;
open Barre;;
open Bouton;;


let reset() = Graphics.set_color Graphics.black;
Graphics.clear_graph();
Graphics.moveto 0 0;;
(* Réinitialise le graphe. *)

let random_color() = Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255);;

let rose = Graphics.rgb 255 100 100;;
let bordeau = Graphics.rgb 200 0 0;;
let orange = Graphics.rgb 255 125 0;;
let gris = Graphics.rgb 200 200 200;;
let vert = Graphics.rgb 13 191 49;;
let marron_fonce = Graphics.rgb 88 41 0;;
let marron_clair = Graphics.rgb 189 95 0;;

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

let degrade_circle x y r color1 color2 =
  let (r1, g1, b1) = traduce_color color1 
  and (r2, g2, b2) = traduce_color color2 in
  let dr = r2 - r1 and dg = g2 - g1 and db = b2 - b1 in
Graphics.set_color color1;
Graphics.plot x y;
for i = 0 to r do 
Graphics.set_color (Graphics.rgb (r1+(i*dr)/r) (g1+(i*dg)/r) (b1+(i*db)/r));
Graphics.draw_circle x y i 
done;;

let draw_boule b = 
degrade_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r) rose bordeau;;
(* Pour dessiner une boule dans la fenêtre graphique. *)

let draw_boule0 b = 
degrade_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r) Graphics.white gris;;
(* Pour dessiner la boule blanche dans la fenêtre graphique. *)

let draw_trou b = 
degrade_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r) Graphics.black marron_fonce;;
(*Graphics.set_color Graphics.black; 
Graphics.fill_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;*)

let draw_close close = 
degrade_circle (int_of_float close.o.x) (int_of_float close.o.y) (int_of_float close.r) Graphics.red orange;
Graphics.set_color Graphics.black;
let x = close.r *. sqrt(2.) *. 0.4 in
let a1 = add_vect close.o {x = x; y = x}
and a2 = add_vect close.o {x = -.x; y = -.x}
and a3 = add_vect close.o {x = -.x; y = x}
and a4 = add_vect close.o {x = x; y = -.x} in
Graphics.draw_segments [|(int_of_float a1.x,int_of_float a1.y,int_of_float a2.x,int_of_float a2.y); (int_of_float a3.x,int_of_float a3.y,int_of_float a4.x,int_of_float a4.y)|];;


let draw_barre barre =
  let xm = Graphics.size_x() and ym = Graphics.size_y() in
degrade 0 xm (ym-40) ym marron_fonce marron_clair;
  let color1 = if fst barre.j2 then Graphics.red else Graphics.blue
  and color2 = if fst barre.j2 then Graphics.blue else Graphics.red in
Graphics.set_color color1;
Graphics.moveto 20 (ym - 25);
Graphics.draw_string ("Joueur 1 : "^(string_of_int (snd barre.j1)));
Graphics.set_color color2;
Graphics.moveto 160 (ym - 25);
Graphics.draw_string ("Joueur 2 : "^(string_of_int (snd barre.j2)));
draw_close barre.close;;
(* On affiche le joueur dont c'est le tour en bleu, et l'autre en rouge. *)

let clear_boule b = Graphics.set_color Graphics.white; 
Graphics.fill_circle(int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
(* Pour effacer une boule de la fenêtre graphique. *)
let draw_billard bill = 
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
  let m = bill.boules in
  let n = bill.n in
reset();
Graphics.set_color vert;
Graphics.fill_rect 0 0 xm ym;
let n1 = Array.length bill.trous in
for i = 0 to n1-1 do 
  draw_trou bill.trous.(i) done;
draw_barre bill.barre;
for i = 1 to n-1 do
  draw_boule m.(i) 
done;
draw_boule0 m.(0)
;;
(* Pour dessiner la configuration actuelle d'un billard dans la fenêtre graphique. *)

let draw_bouton bout = 
degrade bout.xmin bout.xmax bout.ymin bout.ymax gris Graphics.white;
Graphics.set_color Graphics.black;
Graphics.draw_rect bout.xmin bout.ymin (bout.xmax-bout.xmin) (bout.ymax-bout.ymin);
Graphics.moveto (bout.xmin + 10) ((bout.ymin + bout.ymax)/2 - 5);
Graphics.draw_string bout.s;;

let draw_result barre =   
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
degrade (xm/2 - 140) (xm/2 + 140) (ym/2 - 40) (ym/2 + 40) Graphics.white gris;
  let (a,x) = barre.j1 and (b,y) = barre.j2 in
  let i = if x>y then string_of_int 1 else if x<y then string_of_int 2 else "" in
(* i est le numéro du joueur gagnant, ou "" s'il y a égalité *)
Graphics.moveto (xm/2 - 120) (ym/2 + 20);
Graphics.set_color Graphics.black;
if i <> "" then  Graphics.draw_string ("Joueur "^i^" a gagne! Voulez-vous rejouer?")
else Graphics.draw_string ("Egalite! Voulez-vous rejouer?");
let bout_oui = {xmin = xm/2 - 80 ; xmax = xm/2 - 40 ; ymin = ym/2 - 20 ; ymax = ym/2; s = "OUI"}
and bout_non = {xmin = xm/2 + 40 ; xmax = xm/2 + 80 ; ymin = ym/2 - 20 ; ymax = ym/2; s = "NON"} in
draw_bouton bout_oui; draw_bouton bout_non;
while not (select_bouton bout_oui || select_bouton bout_non) do quit_maybe barre done;
select_bouton bout_oui;;
(* Dessine le cadre de résultat. Renvoie true si l'utilisateur veut rejouer, false sinon. *)
