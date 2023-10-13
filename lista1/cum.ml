(*zad 6 uuu*)

let ctrue (a: 'a) (b: 'a) = a
let cfalse (a: 'a) (b: 'a) = b

let cand f g = f g f 
(*let cand f g = fun a b -> if f a b = a then g a b else b*)
let cor f g = f f g
(*let cor f g = fun a b -> if f a b = b then g a b else a*)
let cbool_of_bool p = if p then ctrue else cfalse
let bool_of_cbool f = f true false


(*zad 7 aaaaa*)

(*to co robimy to traktujemy inty jako rekurencyjny typ danych
  i tworzymy naturalnego folda*)
type nat = Succ of nat | Zero  

let rec fold f n = 
  match n with
  | Zero -> (fun x -> x)
  | Succ p -> (fun x ->  f ((fold f p) x))

  (*to wyżej było tak dla jaj*)

let zero f x = x
let succ g = fun f x -> f (g f x)

(* z łączności składanie to basically dodawanie a^b * a^c = a^(b + c) *)
let add l r = fun f x -> l f (r f x) 

(* kocham łączność (a^b)^c = a^(b * c) *) 
let mul l r = fun f x -> l (r f) x 

let is_zero g = g (fun x -> cfalse) ctrue

let rec cnum_of_int n = if n = 0 then zero else succ (cnum_of_int (n - 1))
let int_of_cnum g = g ((+) 1) 0 



