(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de map (int -> 'a), les paires
   clé/valeur sont stockés dans une liste ordonnée sur les clés. *)
(* Définition du type des fonctions des entiers vers 'a *)
type 'a t =
  { assoc : (int * 'a) list;
    default : 'a
  }

(* à compléter *)
let remove_assoc (k: int) (l: (int * 'a) list): (int * 'a) list =
  let rec loop (l:(int * 'a) list) (accu:(int * 'a) list) =
    match l with
    |[] -> List.rev accu
    |(key, v)::xs ->
      if key = k then (List.rev accu)@xs
      else loop xs ((key,v)::accu)
  in loop l []

(* à ne pas toucher *)
let pp_assoc
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (l: (int * 'a) list): unit =
  Format.fprintf fmt "@[[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) l



(* à compléter *)
let constant (d: 'a): 'a t = {assoc = []; default = d}

(* à compléter *)
let find (k: int) (m: 'a t): 'a =
  let rec loop l c =
    match l with
      []->  c
    |(key, v)::xs ->
      if key = k then v
      else if key < k then loop xs c
      else c
  in let l, d = m.assoc, m.default in
  loop l d


(* à compléter *)
let set (k: int) (v: 'a) (m: 'a t): 'a t =
  let l, d = m.assoc, m.default in
  if v = d then {assoc = (remove_assoc k l); default = d}
  else begin
    let rec loop l accu =
      match l with
        []-> {assoc = (List.rev ((k,v)::accu)); default = d}
      |(k1,v1)::xs1 ->
        if k1 = k then {assoc = ((List.rev ((k,v)::accu))@xs1); default = d}
        else begin
          if k1 < k then loop xs1 ((k1,v1)::accu)
          else {assoc = ((List.rev accu)@((k,v)::l)); default = d}
        end
    in loop l []
  end

(* à compléter *)
let fold (f: int -> 'a -> 'b -> 'b) (a: int) (b: int) (m: 'a t) (init: 'b): 'b =
  let rec to_list (x:int) (y:int) (accu:int list): int list=
    if x <= y then to_list (x+1) y (x::accu)
    else accu
  in let interval = to_list a b [] in
  let g = (fun i b -> f i (find i m) b) in
  List.fold_right g interval init

(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m: 'a t): unit =
  Format.fprintf fmt "@[{default:%a;@, assoc:[@[%a@]]@,}@]"
    pp1 m.default
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) m.assoc
