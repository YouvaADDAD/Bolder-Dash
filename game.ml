open Type
open Structures
open Matrix

exception Dead
exception Win

let win (g:game) :bool =
  let i,j = g.player.pos in
  match (Matrix.read i j g.map) with
    Exit(b) -> (g.score >= g.aim) && b
  | _ -> false

(* Compter le nombre de Diamond a l'entree du jeu *)
let count_diamonds (map:cell Matrix.t) :int =
  Matrix.fold (fun _ _ elem nbDia ->
      match elem with
        Diamond -> nbDia + 1
      |_ -> nbDia) map 0

let position_after_move (i, j) (d: dir) =
  match d with
    N -> (i+1, j)
  | S -> (i-1, j)
  | W -> (i, j - 1)
  | E -> (i, j + 1)

let open_exit (g:game) :game =
  if g.score >= g.aim then
    let i,j = g.exit in {g with map = (Matrix.set i j (Exit true) g.map)}
  else g

let valid ((i, j):(int * int)) (game:game) : bool =
  (0 <= i) && (i < game.map.haut) && (j >= 0) && (j < game.map.larg)

let valid_and_empty ((i,j):(int * int)) (game:game) : bool =
  let valid = valid (i,j) game in
  valid && (((Matrix.read i j game.map) = Empty) || ((Matrix.read i j game.map) = Exit false))

(* Renvoyer une liste de tous les cas voisins de (i,j), y inclus (i,j) *)
let voisins (i,j) (g:game) :(int*int) list =
  let l = [(i+1,j-1); (i+1,j); (i+1,j+1);
           (i,j-1); (i,j); (i,j+1);
           (i-1,j-1); (i-1,j); (i-1,j+1)] in
  List.fold_right (fun (x,y) liste ->
      if valid (x,y) g then (x,y)::liste
      else liste) l []

(* Bouger le joueur selon ces regles:
   - ne peut pas faire passer les murs Stone/Magic/steel
   - peut pousser les Boulder/Walnut si le cas suivant le Boulder/Walnut sur la
   direction de mouvement du joeur est Empty, et le joeur ne peut pas le pousser
   vers N
   - peut creuser Dirt
   - manger les Diamonds
   - mort si autour le Butterfly (dans l'un cas voisin de Butterfy)
- *)
let player_turn (g: game) (d: dir) :game =
  let new_xp, new_yp = position_after_move g.player.pos d in
  if (valid (new_xp, new_yp) g) then begin
    let new_map = Matrix.set new_xp new_yp Empty g.map in
    let c = Matrix.read new_xp new_yp g.map in
    let new_world = (match c with
          Stone | Magic | Steel-> g
        |Boulder | Walnut-> (
            match d with
              S|W|E -> begin
                let next_bould_x, next_bould_y = position_after_move (new_xp, new_yp) d in
                if (valid_and_empty (next_bould_x, next_bould_y) g)
                then {g with map = (Matrix.set next_bould_x next_bould_y Boulder new_map);
                          player = {g.player with pos=(new_xp, new_yp)}}
                else g
              end
            |N-> g
          )
        |Dirt -> {g with map = new_map;
                         player = {g.player with pos=(new_xp, new_yp)}}
        |Diamond -> open_exit {g with map = new_map;
                                      player = {g.player with pos=(new_xp, new_yp)};
                                      diamonds_rest =  g.diamonds_rest-1;
                                      score = g.score +1}
        |Empty | Exit(_)-> {g with player = {g.player with pos=(new_xp, new_yp)}}
        |Butterfly(_) -> {g with player = {g.player with pos=(new_xp, new_yp)}}
      ) in
    if List.exists (fun (i,j) -> match (Matrix.read i j new_world.map) with
        | Butterfly(_) -> true
        | _ -> false
      )
        (voisins new_world.player.pos new_world)
    then {new_world with player = {new_world.player with life = false}}
    else new_world
  end
  else g

(* Calculer la position apres la chute d'un Boulder/Diamond/Walnut *)
let position_after_fall (g:game) (i,j) : (int*int) =
  let c = Matrix.read i j g.map in
  match c with
    Boulder | Diamond | Walnut -> begin
      let i_under, j_under = i-1, j in
      match (Matrix.read i_under j_under g.map) with
        Empty -> (i_under, j_under)
      |Boulder | Diamond | Walnut | Stone | Steel | Magic -> (
          if valid_and_empty (i_under, j_under -1) g then (i_under, j_under -1)
          else if valid_and_empty (i_under, j_under +1) g then (i_under, j_under +1)
          else (i,j)
        )
      |_ -> (i,j)
    end
  |_ -> (i,j)


(* Faire exploder le cas (i,j) et tous ses voisins sauf si le cas voisin est
   Steel ou Diamond.
  Si un Walnut est parmi ces cas, il se transforme a` un Diamond
  Si le joueur se situe dans ces cas, il est mort*)
let explode (i,j) (g:game) :game =
  let scaler = Drawing.compute_scaler g in
  let v = voisins (i,j) g in
  let dead = ref false in
  let () = Drawing.reinit_graphics g in
  let new_g = List.fold_right (fun (x,y) (gg:game) ->
      dead := !dead || (gg.player.pos = (x,y));
      match (Matrix.read x y gg.map) with
        Steel | Diamond -> gg
      | Walnut -> {gg with map = Matrix.set x y Diamond gg.map}
      | _ -> (
          Drawing.draw_dead (x,y) scaler;
          {gg with map = Matrix.set x y Empty gg.map}
        )
    ) v g
  in
  let () = Drawing.draw_game new_g scaler in
  Graphics.synchronize ();
  Unix.sleepf 1.0;
  if !dead then {new_g with player = {new_g.player with life = false}}
  else new_g

(* X peut se rouler sur Y si X est parmi {Boulder, Walnut, Diamond} et
   Y est parmi {Boulder, Walnut, Diamond, Stone, Magic, Steel} et la condition
   est valide.
   - si X est un Boulder, X peut tuer le joueur, craquer le Walnut, tuer le Butterfly,
   faire exploder Stone normal, pas Steel, ou transformer Magic a Boulder
   - si X est un Walnut/Diamond, X peut transformer Magic a` Walnut/Diamond
   si le cas dessous la nouvelle position de X est parmi ceux mentionnes ci-dessous
*)
let move_boulder_step (g:game) (i,j) : game =
  let i_new, j_new = position_after_fall g (i,j) in
  if (i_new, j_new) = (i,j) then g
  else begin
    let obj = Matrix.read i j g.map in
    let scale = Drawing.compute_scaler g in
    let new_map = Matrix.set i j Empty g.map in
    match obj with
    | Boulder -> begin
        if g.player.pos = (i_new - 1, j_new)
        then {g with map = Matrix.set i_new j_new Boulder new_map;
                  player = {g.player with life = false}}
        else (
          match (Matrix.read (i_new-1) j_new g.map) with
          | Walnut -> {g with map = (Matrix.set (i_new-1) j_new Diamond (Matrix.set i_new j_new Boulder new_map));
                              diamonds_rest = g.diamonds_rest + 1}
          | Butterfly(_) -> (
              Drawing.redraw_game {g with map = (Matrix.set i_new j_new Boulder new_map)} scale;
              Drawing.draw_dead (i_new -1, j_new) scale;
              Graphics.synchronize (); Unix.sleepf 0.25;
              {g with map = (Matrix.set (i_new-1) j_new Empty (Matrix.set i_new j_new Boulder new_map));
                      diamonds_rest = g.diamonds_rest + 1}
            )
          | Stone -> (
              Drawing.redraw_game {g with map = (Matrix.set i_new j_new Boulder new_map)} scale;
              Unix.sleepf 0.25;
              explode (i_new -1, j_new) {g with map = (Matrix.set i_new j_new Boulder new_map)}
            )
          | Magic -> (
              Drawing.redraw_game {g with map = (Matrix.set i_new j_new Boulder new_map)} scale;
              {g with map = Matrix.set (i_new-1) j_new Boulder (Matrix.set i_new j_new Boulder new_map)}
            )
          | _ -> {g with map = Matrix.set i_new j_new Boulder new_map}
        )
      end
    | Walnut -> begin
        if g.player.pos = (i_new - 1, j_new)
        then {g with map = Matrix.set i_new j_new Walnut new_map}
        else (
          match (Matrix.read (i_new-1) j_new g.map) with
          | Magic -> (
              Drawing.redraw_game {g with map = (Matrix.set i_new j_new Walnut new_map)} scale;
              {g with map = Matrix.set (i_new-1) j_new Walnut (Matrix.set i_new j_new Walnut new_map)}
            )
          | _ -> {g with map = Matrix.set i_new j_new Walnut new_map}
        )
      end
    | Diamond -> begin
        if g.player.pos = (i_new - 1, j_new)
        then open_exit {g with map = new_map; score = g.score +1;
                               diamonds_rest = g.diamonds_rest -1}
        else (
          match (Matrix.read (i_new-1) j_new g.map) with
          | Magic -> (
              Drawing.redraw_game {g with map = (Matrix.set i_new j_new Diamond new_map)} scale;
              {g with map = Matrix.set (i_new-1) j_new Diamond (Matrix.set i_new j_new Diamond new_map);
                      diamonds_rest = g.diamonds_rest +1}
            )
          | _ -> {g with map = Matrix.set i_new j_new Diamond new_map}
        )
      end
    | _ -> g
  end

(* Chercher la position d'un Boulder/Walnut/Diamond qui peut se rouler selon
les regles*)
let find_movable_boulder (g:game) : (int*int)option =
  let f = (fun (i:int) (j:int) (c:cell) (accu:(int*int)option) ->
      match c with
      |Boulder | Diamond | Walnut-> begin
          let i_under, j_under = position_after_move (i,j) S in
          if g.player.pos <> (i_under, j_under) then (
            match (Matrix.read i_under j_under g.map) with
            | Empty -> Some(i,j)
            | Boulder | Diamond | Walnut | Steel | Magic | Stone-> begin
                let i_underleft, j_underleft = position_after_move (i_under, j_under) W in
                let i_underright, j_underright = position_after_move (i_under, j_under) E in
                let i_left, j_left = position_after_move (i,j) W in
                let i_right, j_right = position_after_move (i,j) E in
                if ((valid_and_empty (i_underleft, j_underleft) g) &&
                     (g.player.pos <> (i_underleft, j_underleft)) &&
                     (valid_and_empty (i_left, j_left) g) &&
                     (g.player.pos <> (i_left, j_left)))
                    ||
                    ((valid_and_empty (i_underright, j_underright) g) &&
                     (g.player.pos <> (i_underright, j_underright)) &&
                     (valid_and_empty (i_right, j_right) g) &&
                     (g.player.pos <> (i_right, j_right)))
                then Some(i,j)
                else accu
              end
            | _ -> accu
          )
          else accu
        end
      |_ -> accu
    ) in
  Matrix.fold f g.map None

let world_turn (g: game) :game =
  let scaler = Drawing.compute_scaler g in
  let r = ref (find_movable_boulder g) in
  let new_world = ref g in
  while !r <> None do
    (match !r with
       Some (i,j) -> (new_world:=move_boulder_step !new_world (i,j);
                      Drawing.redraw_game !new_world scaler;
                      Unix.sleepf 0.15;
                      r:= find_movable_boulder !new_world;
                     )
     |None -> ());
  done;
  !new_world


(* Definir le mouvement d'un Butterfy
Butterfly se bouge dans le sens clockwise le long de la bordue des espaces vides*)
let rec butterfly_turn (i,j) (g:game) :game =
  let new_map = Matrix.set i j Empty g.map in
  let surrounded:bool = (List.fold_right
      (fun (ii,jj) b ->
        if (ii,jj) <> (i,j) then b && (not(valid_and_empty (ii,jj) g))
        else b)
      (voisins (i,j) g) true) in
  if surrounded then g
  else (
    match (Matrix.read i j g.map) with
      Butterfly(d) -> begin
        match d with
          N -> if (valid_and_empty (i+1,j) g) && (valid_and_empty (i, j-1) g)
          then {g with map = (Matrix.set i (j-1) (Butterfly W) new_map)}
          else if (valid_and_empty (i+1,j) g)
          then {g with map = (Matrix.set (i+1) j (Butterfly N) new_map)}
          else butterfly_turn (i,j) {g with map = (Matrix.set i j (Butterfly E) new_map)}
        | S -> if (valid_and_empty (i-1,j) g) && (valid_and_empty (i, j+1) g)
          then {g with map = (Matrix.set i (j+1) (Butterfly E) new_map)}
          else if (valid_and_empty (i-1,j) g)
          then {g with map = (Matrix.set (i-1) j (Butterfly S) new_map)}
          else butterfly_turn (i,j) {g with map = (Matrix.set i j (Butterfly W) new_map)}
        | E -> if (valid_and_empty (i,j+1) g) && (valid_and_empty (i+1, j) g)
          then {g with map = (Matrix.set (i+1) j (Butterfly N) new_map)}
          else if (valid_and_empty (i,j+1) g)
          then {g with map = (Matrix.set i (j+1) (Butterfly E) new_map)}
          else butterfly_turn (i,j) {g with map = (Matrix.set i j (Butterfly S) new_map)}
        | W -> if (valid_and_empty (i,j-1) g) && (valid_and_empty (i-1, j) g)
          then {g with map = (Matrix.set (i-1) j (Butterfly S) new_map)}
          else if (valid_and_empty (i,j-1) g)
          then {g with map = (Matrix.set i (j-1) (Butterfly W) new_map)}
          else butterfly_turn (i,j) {g with map = (Matrix.set i j (Butterfly N) new_map)}
      end
    | _ -> g
  )

(* Faire bouger tous les Butterfy existant dans le plateau du jeu actuel *)
let all_butterfly_turn (g:game) :game =
  let f = (fun i j elem gg ->
      match elem with
        Butterfly(_) -> butterfly_turn (i,j) gg
      | _ -> gg
    )
in Matrix.fold f g.map g
