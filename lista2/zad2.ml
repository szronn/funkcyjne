let rec either_map f xs = 
  match xs with
  | [] -> []
  | x :: xs -> (f x) :: x :: (either_map f xs)


let rec sublists xs = 
  match xs with
  | [] -> [[]]
  | x :: xs -> either_map (List.cons x) (sublists xs)


