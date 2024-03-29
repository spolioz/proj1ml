open Vecteur;;
open Niveau;;
open Boule2;;
open Barre2;;
open Bouton;;
open Obstacle;;

let reset() = Graphics.set_color Graphics.black;
Graphics.clear_graph();
Graphics.moveto 0 0;;
(* Réinitialise le graphe. *)

let random_color() = Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255);;
(* Renvoie une couleur aléatoire. *)

let rose = Graphics.rgb 255 100 100;;
let bordeau = Graphics.rgb 200 0 0;;
let orange = Graphics.rgb 255 100 20;;
let gris = Graphics.rgb 200 200 200;;
let vert = Graphics.rgb 13 191 49;;
let marron_fonce = Graphics.rgb 88 41 0;;
let marron_clair = Graphics.rgb 189 95 0;;
let ciel = Graphics.rgb 117 117 255;;
let gris_fonce = Graphics.rgb 80 90 90;;
(* Définition de couleurs qui seront utilisées dans les fonctions suivantes. *)

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
(* Remplit un rectangle décrit par ses coordonées extrémales avec un dégradé entre color1 et color2. *)

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
(* Même chose avec un cercle. *)

let draw_boule b = 
degrade_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r) Graphics.green vert;;
(* Pour dessiner une boule dans la fenêtre graphique. *)

let draw_boule0 b = 
degrade_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r) rose bordeau;;
(* Pour dessiner la boule projectile dans la fenêtre graphique. *)

let draw_close close = 
degrade_circle (int_of_float close.o.x) (int_of_float close.o.y) (int_of_float close.r) Graphics.red orange;
Graphics.set_color gris_fonce;
let x = close.r *. sqrt(2.) *. 0.2 in
let a1 = add_vect close.o {x = x; y = x}
and a2 = add_vect close.o {x = -.x; y = -.x}
and a3 = add_vect close.o {x = -.x; y = x}
and a4 = add_vect close.o {x = x; y = -.x} in
Graphics.draw_segments [|(int_of_float a1.x,int_of_float a1.y,int_of_float a2.x,int_of_float a2.y); (int_of_float a3.x,int_of_float a3.y,int_of_float a4.x,int_of_float a4.y)|];
Graphics.draw_circle (int_of_float close.o.x) (int_of_float close.o.y) (int_of_float close.r);;
(* Dessine le bouton Close de la barre. *)

let draw_reset r =
degrade_circle (int_of_float r.o.x) (int_of_float r.o.y) (int_of_float r.r) Graphics.blue ciel;
Graphics.set_color gris_fonce;
Graphics.draw_circle  (int_of_float r.o.x) (int_of_float r.o.y) (int_of_float r.r);
Graphics.set_color Graphics.red;
Graphics.moveto ((int_of_float r.o.x)-2) ((int_of_float r.o.y) - 6);
Graphics.draw_string "R";;
(* Même chose pour le bouton reset.*)

let draw_barre barre =
  let xm = Graphics.size_x() and ym = Graphics.size_y() in
degrade 0 xm (ym-40) ym marron_fonce marron_clair;
Graphics.set_color Graphics.red;
Graphics.moveto 20 (ym - 25);
Graphics.draw_string ("Score : "^(string_of_int barre.score));
draw_reset barre.reset;
draw_close barre.close;;
(* Dessine la barre dans la fenêtre graphique. *)

let draw_obstacle ob = let (xmin,ymin,xmax,ymax,b) = ob in
Graphics.set_color gris_fonce;
Graphics.fill_rect (int_of_float xmin) (int_of_float ymin) (int_of_float (xmax -. xmin)) (int_of_float (ymax -. ymin));
Graphics.fill_circle (int_of_float b.o.x) (int_of_float b.o.y) (int_of_float b.r);;
(* Dessine un obstacle. *)

let draw_niveau niv = 
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
  let m = niv.boules in
  let n = niv.n in
reset();
degrade 0 xm 0 ym Graphics.white ciel;
for i = 1 to n-1 do
  draw_boule m.(i) done;
draw_boule0 m.(0);
draw_barre niv.barre;
List.iter draw_obstacle niv.obs;;
(* Pour dessiner la configuration actuelle d'un niveau dans la fenêtre graphique. *)

let draw_bouton bout = 
degrade bout.xmin bout.xmax bout.ymin bout.ymax gris Graphics.white;
Graphics.set_color Graphics.black;
Graphics.draw_rect bout.xmin bout.ymin (bout.xmax-bout.xmin) (bout.ymax-bout.ymin);
Graphics.moveto (bout.xmin + 10) ((bout.ymin + bout.ymax)/2 - 5);
Graphics.draw_string bout.s;;
(* Dessine le bouton bout. *)

let draw_result niv =
  let barre = niv.barre in   
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
degrade (xm/2 - 140) (xm/2 + 140) (ym/2 - 40) (ym/2 + 40) Graphics.white gris;
  let i = niv.n in
(* i est le numéro du joueur gagnant, ou "" s'il y a égalité *)
Graphics.moveto (xm/2 - 130) (ym/2 + 25);
Graphics.set_color Graphics.black;
if i > 1 then  Graphics.draw_string ("Level failed... Try again?")
else (
  Graphics.draw_string ("You won! Your score is "^string_of_int(barre.score)^"!");
  Graphics.moveto (xm/2 - 130) (ym/2 + 10);
  Graphics.draw_string "Would you like to play again?" );
let bout_oui = {xmin = xm/2 - 80 ; xmax = xm/2 - 40 ; ymin = ym/2 - 20 ; ymax = ym/2; s = "YES"}
and bout_non = {xmin = xm/2 + 40 ; xmax = xm/2 + 80 ; ymin = ym/2 - 20 ; ymax = ym/2; s = "NO"} in
draw_bouton bout_oui; draw_bouton bout_non;
while not (select_bouton bout_oui || select_bouton bout_non) do
  quit_maybe barre done;
  if select_bouton bout_non then raise Close
  else raise Reset;;
(* Dessine le cadre de résultat. Renvoie l'exception Close si l'utilisateur a répondu NON, ou s'il a cliqué sur le bouton Close, et Reset s'il a répondu OUI. *)


let draw_reset_choice barre =
  let xm = Graphics.size_x() 
  and ym = Graphics.size_y() in
degrade (xm/2 - 150) (xm/2 + 150) (ym/2 - 60) (ym/2 + 20) Graphics.white gris;
Graphics.moveto (xm/2 - 140) (ym/2);
Graphics.set_color Graphics.black;
Graphics.draw_string "Which level do you want to try?";
let bout1 = {xmin = xm/2 - 80 ; xmax = xm/2 - 40 ; ymin = ym/2 - 40 ; ymax = ym/2 - 20; s = " 1 "}
and bout2 = {xmin = xm/2 + 40 ; xmax = xm/2 + 80 ; ymin = ym/2 - 40 ; ymax = ym/2 - 20; s = " 2 "} in
draw_bouton bout1; draw_bouton bout2;
while not (select_bouton bout1 || select_bouton bout2) do
  quit_maybe barre;
done;
if select_bouton bout1 then 1
else 2;;
(* Affiche une fenêtre de choix de niveau, et renvoie l'entier correspondant au niveau choisi par l'utilisateur. *)
