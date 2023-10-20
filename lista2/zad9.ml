
type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = { clist = fun f z -> z }
let ccons x ys = { clist = fun f z -> f x (ys.clist f z) }
let map g xs = { clist = fun f z -> xs.clist (fun a x -> f (g a) x) z }
let append xs ys = { clist = fun f z -> xs.clist f (ys.clist f z) }
let mult xs ys = { clist = fun f z -> xs.clist (fun a x -> ys.clist (fun b y -> f (a, b) y) x) z }
let clist_to_list xs = xs.clist List.cons []
let rec clist_of_list xs =
  match xs with
  | [] -> cnil
  | x :: xs -> ccons x (clist_of_list xs)
