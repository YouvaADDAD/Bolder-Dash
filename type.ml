(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

open Structures

(** Ce fichier contient les définitions des types principaux du jeu *)
type dir = N | S | W | E

type cell = Stone | Boulder | Dirt | Diamond | Empty
          | Walnut | Exit of bool
          | Magic | Steel | Butterfly of dir

(* TODO *)
let string_of_cell (c:cell) : string =
  match c with
    Stone -> "Stone"
  |Boulder -> "Boulder"
  |Dirt -> "Dirt"
  |Diamond -> "Diamond"
  |Empty -> "Empty"
  |Walnut -> "Walnut"
  |Exit(b) ->
    if b then "Exit Opened"
    else "Exit Closed"
  | Magic -> "Magic Wall"
  | Steel -> "Steel Wall"
  | Butterfly(_) -> "Butterfy"

let pp_cell fmt cell = Format.pp_print_string fmt (string_of_cell cell)

type map =
  cell Matrix.t

let pp_map : Format.formatter -> map -> unit = Matrix.pp pp_cell

type player = {
  pos: (int*int);
  life: bool;
}
type game =
{
  map: map;
  player: player;
  diamonds_rest : int;
  score: int;
  aim:  int;
  exit: int*int;
}

let print_game (fmt: Format.formatter) (g: game): unit =
  Format.fprintf fmt "@[<v>@[<v 2>{@,map: %a@,player: (%d, %d)@,}@]@,@]"
    pp_map g.map
    (fst g.player.pos) (snd g.player.pos)



let string_of_dir (d: dir) =
  match d with
    N -> "North"
  | S -> "South"
  | W -> "West"
  | E -> "East"

let pp_dir fmt dir = Format.pp_print_string fmt (string_of_dir dir)
