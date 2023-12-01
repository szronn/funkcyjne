
let rec fold_left_cps f a xs cnt =
  match xs with
  | []      -> cnt a
  | x :: xs -> f a x (fun k -> fold_left_cps f k xs cnt)

let id x = x

let fold_left f a xs = fold_left_cps f a xs id