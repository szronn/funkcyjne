

type cbool = { cbool : 'a. 'a -> 'a -> 'a }
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

(* ale fajne *)
let even n =
  { cbool = fun tt ff -> fst (n.cnum (fun (a, b) -> (b, a)) (tt, ff)) }


let ctrue = { cbool = fun a b -> a }
let cfalse = { cbool = fun a b -> b }

let cand f g = f.cbool g f 
(*let cand f g = { cbool = fun a b -> if f.cbool true false then g.cbool a b else b } *)
let cor f g = f.cbool f g
(* let cor f g = { cbool = fun a b -> if f.cbool false true then g.cbool a b else a } *)
let cbool_of_bool p = if p then ctrue else cfalse
let bool_of_cbool f = f.cbool true false

let zero = { cnum = fun f x -> x }
let succ g = { cnum = fun f x -> f (g.cnum f x) }
let add l r = { cnum = fun f x -> l.cnum f (r.cnum f x) }
let mul l r = { cnum = fun f x -> l.cnum (r.cnum f) x }
let is_zero g = g.cnum (fun x -> cfalse) ctrue
let rec cnum_of_int n = if n = 0 then zero else succ (cnum_of_int (n - 1))
let int_of_cnum g = g.cnum ((+) 1) 0 