(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de matrice implanté à l'aide de
    maps. *)

(* Définition de type *)
type 'a t =
  {
    larg    : int;
    haut    : int;
    map     : ('a Assoc.t) Assoc.t;
  }

(* à compléter ici: définition de l'exception Out_of_bounds *)
exception Out_of_bounds
(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m  : 'a t): unit =
  Format.fprintf fmt "@[<v 2>{@,larg:%d@,haut:%d@,m:%a}}@]"
    m.larg
    m.haut
    (Assoc.pp (Assoc.pp pp1)) m.map

(* à compléter *)
let make (haut: int) (larg: int) (default: 'a): 'a t=
  {larg = larg; haut = haut; map = Assoc.constant (Assoc.constant default)}

let read (i: int) (j: int) (m: 'a t): 'a =
  if i < 0 || i >= m.haut || j < 0 || j >= m.larg then raise Out_of_bounds
  else Assoc.find j (Assoc.find i m.map)

let set (i: int) (j: int) (v: 'a) (m: 'a t): 'a t =
  if i < 0 || i >= m.haut || j < 0 || j >= m.larg then raise Out_of_bounds
  else {m with map = (Assoc.set i (Assoc.set j v (Assoc.find i m.map)) m.map)}

let fold (f: int -> int -> 'a -> 'b -> 'b) (m: 'a t) (acc: 'b): 'b =
  Assoc.fold (fun i ligne accu1 ->
      Assoc.fold (fun j elem accu2 -> f i j elem accu2) 0 (m.larg - 1) ligne accu1
    ) 0 (m.haut-1) m.map acc

let iter (f: int -> int -> 'a -> unit) (m: 'a t): unit =
  fold (fun i j elem () -> f i j elem) m ()
