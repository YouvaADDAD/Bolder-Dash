(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient les fonctions d'affichage graphique du jeu *)

open Graphics
open Structures
open Type

type scale = (int * int) -> (int * int)

let gray = rgb 105 105 105
let brown = rgb 139 69 19
let diamond = rgb 185 242 255
let steel = rgb 176 196 222
let magic_wall = rgb 0 139 139
let butterfy = rgb 0 255 127

let compute_scaler (g: game): scale =
  let sx = size_x () in
  let sy = size_y () in
  let rx = (sx - sx / 5) / g.map.larg in
  let ry = (sy - sy / 5) / g.map.haut in
  let sxi = rx * g.map.larg in
  let syi = ry * g.map.haut in
  let margx = (sx - sxi) / 2 in
  let margy = (sy - syi) / 2 in
  fun (x, y) ->
    (margx + x * rx, margy + y * ry)

(* à compléter *)
let draw_rect_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  let x1, y1 = scaler (j,i) in
  let x2, y2 = scaler (j+1, i+1) in
  let wcell, hcell = (x2-x1),(y2-y1) in
  set_color c;
  draw_rect y1 x1 wcell hcell

(* à compléter *)
let fill_rect_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  let x1, y1 = scaler (j,i) in
  let x2, y2 = scaler (j+1, i+1) in
  let wcell, hcell = (x2-x1),(y2-y1) in
  set_color c;
  fill_rect x1 y1 wcell hcell

(* à compléter *)
let fill_diamond_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  let x1, y1 = scaler (j,i) in
  let x2, y2 = scaler (j+1, i+1) in
  let wcell, hcell = (x2-x1),(y2-y1) in
  let vertices = [(x1, y1+hcell/2);
                  (x1+wcell/2, y1);
                  (x1+wcell, y1+hcell/2);
                  (x1+wcell/2, y1+hcell)] in
  set_color c;
  fill_poly (Array.of_list vertices)

(* à compléter *)
let fill_circle_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  let x1, y1 = scaler (j,i) in
  let x2, y2 = scaler (j+1, i+1) in
  let wcell, hcell = (x2-x1),(y2-y1) in
  let x, y = x1+wcell/2, y1+hcell/2 in
  let rx, ry = wcell/2, hcell/2 in
  set_color c;
  fill_ellipse x y rx ry


let draw_dead ((i,j):(int*int)) (scaler:scale) :unit =
  let x00, y00 = scaler (j,i) in
  let x01, y01 = scaler (j+1, i+1) in
  let x10, y10 = scaler (j, i+1) in
  let x11, y11 = scaler (j+1, i) in
  let l = [(x00, y00, x01, y01); (x10, y10, x11, y11)] in
  set_line_width 5;
  set_color black;
  draw_segments (Array.of_list l)

let draw_butterfly c (i,j) scaler :unit =
  let x1, y1 = scaler (j,i) in
  let x2, y2 = scaler (j+1, i+1) in
  let wcell, hcell = (x2-x1),(y2-y1) in
  let xc, yc = x1+wcell/2+1, y1+hcell/2+1 in
  let vertices1 = [(x1, y1); (x1,y2); (xc,yc)] in
  let vertices2 = [(x2, y1); (x2, y2); (xc,yc)] in
  set_color c;
  fill_poly (Array.of_list vertices1);
  fill_poly (Array.of_list vertices2)

(* à compléter *)
let draw_cell (c: cell) (i, j) (scaler: scale): unit =
  match c with
    Stone -> fill_rect_cell black (i,j) scaler
  |Boulder -> fill_circle_cell gray (i,j) scaler
  |Dirt -> fill_rect_cell brown (i,j) scaler
  |Diamond -> fill_diamond_cell diamond (i,j) scaler
  |Empty -> ()
  |Walnut -> fill_circle_cell yellow (i,j) scaler
  |Exit(b) ->
    if b then fill_rect_cell magenta (i,j) scaler
    else ()
  | Magic -> fill_rect_cell magic_wall (i,j) scaler
  | Steel -> fill_rect_cell steel (i,j) scaler
  | Butterfly(_) -> draw_butterfly butterfy (i,j) scaler

(* à compléter *)
let draw_map (m: map) (scaler: scale): unit =
  Matrix.iter (fun i j c -> draw_cell c (i,j) scaler) m

(* à compléter *)
let draw_player ((i, j): (int * int)) (scaler: scale): unit =
  fill_circle_cell red (i,j) scaler

let draw_diamonds_score (g:game) (scaler:scale) :unit =
  let x, y = scaler (0, g.map.haut) in
  set_font ("-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1");
  set_color red;
  moveto x y;
  draw_string ("Diamonds: " ^ (string_of_int g.diamonds_rest));
  set_color green;
  draw_string ("  Score: " ^ (string_of_int g.score) ^
               "|" ^ (string_of_int g.aim))

(* à compléter *)
let draw_game (g: game) (scaler: scale) =
  draw_map g.map scaler;
  draw_player g.player.pos scaler;
  draw_diamonds_score g scaler;
  synchronize ()

(* à compléter *)
let init_graphics g: unit =
  open_graph "";
  set_window_title "Boulder Dash";
  resize_window (30*g.map.larg) (30*g.map.haut);
  auto_synchronize false

(* à compléter *)
let reinit_graphics g: unit =
  clear_graph ();
  init_graphics g

let redraw_game (g:game) (scaler:scale) =
  reinit_graphics g;
  draw_game g scaler
