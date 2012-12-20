open Graphics;;

type bouton = {xmin : int; xmax : int; ymin : int; ymax : int; s : string};;

let select_bouton bout =
  let (x,y) = Graphics.mouse_pos() in
 Graphics.button_down() && x >= bout.xmin && x <= bout.xmax && y >= bout.ymin && y <= bout.ymax;;
