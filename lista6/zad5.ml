
let rec fold_left_cps f a xs cnt =
  match xs with
  | []      -> cnt a
  | x :: xs -> f a x (fun k -> fold_left_cps f k xs cnt)

let id x = x

let fold_left f a xs = fold_left_cps f a xs id


let for_all p xs =
  fold_left_cps (fun acc x cnt ->
                if not (p x)
                then false
                else cnt (p x))
  true
  xs
  id

let mult_list xs =
  fold_left_cps (fun acc x cnt ->
                if x = 0
                then 0
                else cnt (acc * x))
  1
  xs
  id


let sorted xs = 
  fold_left_cps (fun acc x cnt ->
                if acc > x
                then false
                else cnt x)
  0
  xs
  (fun _ -> true)


               