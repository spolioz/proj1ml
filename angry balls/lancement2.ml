open Vecteur;;
open Boule2;;
open Barre2;;
open Niveau;;
open Graphique2;;
open Obstacle;;

let launch niv = 
  let nb = ref niv.n in
(* nb représente le nombre de boules dans le niveau. *)
  let i = ref 0 in
(* i représente le nombre de boules supprimées pendant le tour *) 
draw_niveau niv;
while (niv.n > 1 && (vit_max niv > 40. || not (Graphics.button_down()))) do
(* On autorise le changement de tour quand la vitesse des boules est suffisamment
faible, auquel cas on le propose à l'utilisateur, qui clique pour passer au
tour suivant. On arrête sans rien demander quand il n'y a plus que la boule
projectile dans le niveau. *)
  reset_maybe niv.barre;
  quit_maybe niv.barre;
(* Si on a cliqué sur le bouton close, on ferme tout, ou reset, et on recommence tout. *)
  let j = !nb - niv.n in
(* On compte le nombre de boules supprimées au cours de la dernière évolution *)
  incr_score (niv.barre) (j*(!i+1 + !i+j)*50);
(* On incrémente le score du joueur en fonction : 100 points pour la première boule supprimée, 200 pour la suivante, etc... *)
  nb := niv.n; i := !i + j;
  evolution niv;
(* On fait évoluer le niveau d'une frame. *)
  Graphics.auto_synchronize false;
  draw_niveau niv;
  if vit_max niv <= 40. then begin
    Graphics.moveto (Graphics.size_x()/2 - 50) (Graphics.size_y()/2);
    Graphics.set_color Graphics.red;
    Graphics.draw_string "NEXT TURN OK : CLICK TO PASS"
(* Affichage de la proposition de passage au tour suivant lorsque les conditions
le permettent. *)
  end;
  Graphics.auto_synchronize true;
done;
let j = !nb - niv.n in
incr_score (niv.barre) (j*(!i+1 + !i+j)*50);
(* Si on est sorti de la boucle parce que la dernière boule a été détruite, il faut quand même calculer le nouveau score! *)
Graphics.auto_synchronize false;
draw_niveau niv;
(* On redessine le niveau pour que le changement de score s'affiche. *)
Graphics.auto_synchronize true;;
(* Lance et affiche l'évolution du niveau, tant que la vitesse des boules reste raisonnable. Alors, l'utilisateur peut passer au tour suivant en cliquant. *)

let random_boule r = 
  let xm = float_of_int (Graphics.size_x())
  and ym = float_of_int (Graphics.size_y()) in
{o={x=Random.float (xm -.r); y=Random.float(ym-.r)}; 
r=r; s=r*.r*.r; v={x=0.; y=0.}; a={x=0.; y=0.}};;
(* Crée une boule placée au hasard dans le niveau, 
de rayon r, sans vitesse ni accélération. *)

let rec random_boule_liste n r = 
if n=0 then []
else (random_boule r) :: (random_boule_liste (n-1) r);;
(* Une liste de n boules de rayon r placées aléatoirement. *)

let make_rangee_boule x n r =
  let ymin = -.r in
     let rec aux i l = 
       if i = 0 then l 
       else aux (i-1) ({o={x=x; y=float_of_int(i)*.2.*.r +. ymin}; r=r; s=1000.; v={x=0.; y=0.}; a={x=0.; y=0.}} :: l) in
aux (n) [];;
(* Renvoie une rangée verticale de n boules de rayon r, 
placées selon l'abscisse x, et touchant le sol. *)

let make_rangee_boule2 y n r =
  let xm = float_of_int (Graphics.size_x()) in
  let n' = float_of_int n in
if n' *. r > xm then failwith "Le niveau est trop petit pour accueillir autant de boules!"
else let xmin = (3. *. xm /. 4.) -. ((n' +. 1.) *. r) in
     let rec aux i l = 
       if i = 0 then l 
       else aux (i-1) ({o={x=float_of_int(i)*.2.*.r +. xmin; y=y}; r=r; s=2000.; v={x=0.; y=0.}; a={x=0.; y=0.}} :: l) in
aux (n) [];;
(* Renvoie une rangée horizontale de n boules de rayon r, 
placées selon l'ordonnée y, et centrée au 3/4 du niveau. *)

let make_colonnes_boule n r =
  let xm = float_of_int (Graphics.size_x()) in
  let ym = float_of_int (Graphics.size_y()) in
  let rec aux i l = 
    if i=0 then (make_rangee_boule (xm -. 5.*.r) n r) @ l
    else aux (i-1) ((make_rangee_boule (xm -. 5.*.r -. (float_of_int i)*.2.*.r) (n-i) r) @ l) in
{o={x=5.*.r; y=ym/.2.}; r=r*.0.8; s=infinity; v={x=0.; y=0.}; a={x=0.; y=0.}}::(aux n []);;
(* Renvoie n colonnes de boules de rayon r, de tailles croissantes. *)

let make_triangle_boule n r =
  let ym = float_of_int (Graphics.size_y()) in
  let rec aux i l = 
    if i=0 then (make_rangee_boule2 r n r) @ l
    else aux (i-1) ((make_rangee_boule2 (r+.(float_of_int i)*.(sqrt 3.)*.r) (n-i) r) @ l) in
{o={x=5.*.r; y=ym/.2.}; r=r*.0.8; s=infinity; v={x=0.; y=0.}; a={x=0.; y=0.}}::(aux n []);;
(* Renvoie un triangle de boules, placées en une pyramide de n rangées de haut. *)

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
  let niv = {boules = m; n=n; obs = []; barre=barre} in
draw_niveau niv;
niv
end;;
(* Crée un niveau contenant les boules de la liste l. *)

let initialise = 
let xm = float_of_int (Graphics.size_x()) in
let ob1 = [make_obstacle (xm /. 2.) 0. 40. 400.]
and ob2 = [make_obstacle (xm /. 2.) 0. 40. 60. ; make_obstacle (xm -. 40.) 0. 40. 60.]
in function
  |1 -> let l = make_colonnes_boule 5 30. in
	let niv = make_niveau l in
	niv.obs <- ob1; niv
  |2 -> let l = make_triangle_boule 9 29. in
	let niv = make_niveau l in
	niv.obs <- ob2; niv
  |_ -> failwith "Level not implemented";;
(* Prend en entrée le numéro de niveau souhaité, et l'initialise. *)
