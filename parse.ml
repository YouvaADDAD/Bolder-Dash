open Structures
open Type

(* Fonctions pour generer une instance de jeu *)
(* let random_between (a:int) (b:int) :int =
  (Random.int (b-a+1)) + a

let random_wall () :cell =
  let w = random_between 1 3 in
  if w = 1 then Magic
  else if w = 2 then Steel
  else Stone

let random_diamond ():cell =
  let d = random_between 1 3 in
  if d = 1 then Walnut
  else Diamond

let random_map h l aim :map =
  let m = ref (Matrix.make h l Empty) in
  let number = ref (-1) in
  let obj = ref (Empty) in
  let nbDia = ref (random_between aim (aim+20)) in
  let nbBut = ref (random_between 0 ((l-h)/h+1)) in
  let setExit = ref false in
  for i = h - 1 downto 0 do
    for j = 0 to l-1 do
      if i=0 || i = h-1 || j=0 || j = l-1 then m := Matrix.set i j Steel !m
      else ();
    done
  done;
  !m

let char_of_cell (c:cell) :char =
  match c with
  | Empty -> ' '
  | Boulder -> 'O'
  | Stone -> 'X'
  | Diamond -> 'V'
  | Exit(_) -> 'P'
  | Walnut -> 'W'
  | Magic -> 'M'
  | Steel -> 'S'
  | Butterfly(_) -> 'B'
  | Dirt -> '-'

let make_game (name:string) :unit =
  let oc = open_out name in
  let h = random_between 20 40 in
  let l = random_between 40 80 in
  let aim_score = random_between (l-h) (l+h) in
  let m = random_map h l aim_score in
  output_string oc (string_of_int h);
  output_string oc (string_of_int l);
  output_string oc (string_of_int aim_score);
  for i = h - 1 downto 0 do
    for j = 0 to l-1 do
      output_char oc (char_of_cell (Matrix.read i j m));
    done
  done;
  close_out oc *)

(* Fonctions pour lire une instance de jeu du fichier *)
let cell_of_char (c:char): cell option =
  match c with
  | ' ' -> Some(Empty)
  | 'O' -> Some(Boulder)
  | 'X' -> Some(Stone)
  | '-' -> Some(Dirt)
  | 'V' -> Some (Diamond)
  | 'P' -> None
  | 'W' -> Some(Walnut)
  | 'M' -> Some(Magic)
  | 'S' -> Some(Steel)
  | 'B' -> Some(Butterfly(N))
  | _ -> (
      print_char c;
      failwith "unrecognized char"
    )

let parse_file (f: string) =
  let ic = open_in f in
  let haut = int_of_string (input_line ic) in
  let larg = int_of_string (input_line ic) in
  let aim_score = int_of_string (input_line ic) in
  let joueur = ref (0,0) in
  let map = ref (Matrix.make haut larg Empty) in
  (* i : ligne (inversion à cause de Graphics, j : colonne *)
  for i = haut - 1 downto 0 do
    let line = input_line ic in
    for j = 0 to larg - 1 do
      match cell_of_char line.[j] with
      | None -> begin joueur := (i,j); map := Matrix.set i j (Exit false) !map end
      | Some(Butterfly(_)) -> begin
          let c_above = Matrix.read (i+1) j !map in
          let c_under = Matrix.read (i-1) j !map in
          if c_above = Empty || c_above = Exit(false)
          then map := Matrix.set i j (Butterfly E) !map
          else if c_under = Empty || c_under = Exit(false)
          then  map := Matrix.set i j (Butterfly W) !map
      end
      | Some b -> map := Matrix.set i j b !map;
    done
  done;
  let nbDia = Game.count_diamonds !map in
  { player = {pos = !joueur; life = true};
    map = !map ;
    diamonds_rest = nbDia;
    score = 0; aim = aim_score;
    exit = !joueur}
