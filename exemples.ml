open Vecteur;;
open Boule;;
open Trou;;
open Billard;;
open Graphique;;
open Lancement;;
open Interface;;

let b1 = {o={x=200.;y=60.}; r=20.; m=8000.; v ={x=Random.float 3000. ;y=Random.float 3000.}; a = {x=0.; y=0.}};;
let b2 = {o={x=180.;y=180.}; r=20.; m=125000.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let b3 = {o={x=300.;y=300.}; r=20.; m=125000.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;

let trou1 = {o={x=0.;y=0.}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let trou2 = {o={x=float_of_int (Graphics.size_x());y=0.}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let trou3 = {o={x=float_of_int (Graphics.size_x());y=float_of_int (Graphics.size_y())}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;
let trou4 = {o={x=0.;y=float_of_int (Graphics.size_y())}; r=50.; m=0.; v ={x=0.;y=0.}; a = {x=0.; y=0.}};;

let m = {boules = [|b1;b2;b3|]; n=3; f=0.999; trous = [|trou1; trou2; trou3; trou4|]};;

Graphics.open_graph "";;

let l = make_triangle_boule 6 20.;;

let m = make_billard l;;

partie m;;
