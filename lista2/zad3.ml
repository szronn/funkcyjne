

let rec suffixes xs = 
  match xs with
  | [] -> [[]]
  | x :: xs -> (x :: xs) :: (suffixes xs)

(* let prefixes xs = List.rev (suffixes (List.rev xs)) *)

let rec prefixes' xs = 
  match xs with
  | [] -> []
  | x :: xs -> [x] :: List.map (List.cons x) (prefixes' xs)

let prefixes xs = [] :: prefixes' xs





