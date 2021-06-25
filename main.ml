open Boulder_dash
open Type
open Graphics
open Game

let message color message =
  moveto ((size_x ())/2-50) ((size_y ())/2-50);
  set_color color;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  draw_string message;
  synchronize ();
  Unix.sleepf 3.0

let handle (st: Graphics.status) (g: game):game =
  let scaler = Drawing.compute_scaler g in
  if st.keypressed then begin
    let new_world = ref (Game.all_butterfly_turn g) in
       new_world := (match st.key with
             'w'-> (Game.player_turn !new_world N)
           |'s' -> (Game.player_turn !new_world S)
           |'a' -> (Game.player_turn !new_world W)
           |'d' -> (Game.player_turn !new_world E)
           |'x' -> raise Exit
           |_ -> !new_world
         );
       Drawing.redraw_game !new_world scaler;
       (if Game.win !new_world then raise Win
        else new_world := Game.world_turn !new_world);
       if (!new_world.player.life = false)
       then (
         Drawing.draw_dead !new_world.player.pos scaler;
         raise Dead
       )
       else !new_world
  end
  else g

let rec turn g =
  let st = wait_next_event [Key_pressed] in
  try (let gg = handle st g in
  let scale = Drawing.compute_scaler g in
  let () = Drawing.redraw_game gg scale in
  turn gg)
  with
  |Win -> raise Win
  |Dead -> raise Dead

let game ()  =
  let game = Parse.parse_file "data/level3.lv" in
  Drawing.init_graphics game;
  let scale = Drawing.compute_scaler game in
  Drawing.draw_game game scale;
  let new_g = Game.world_turn game in
  Drawing.redraw_game new_g scale;
  try turn new_g
  with
  |Win ->  message green "GAGNE"
  |Dead -> message red "PERDU"

let () =
  game ()
