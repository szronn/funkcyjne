
type 'a ziplist = 'a list * 'a list

let ziplist_of_list xs = (xs, [])
let list_of_ziplist (xs, ys) = List.fold_left (fun acc x -> x :: acc) xs ys
let left (xs, ys) = (List.tl xs, List.hd xs :: ys)
let right (xs, ys) = (List.hd ys :: xs, List.tl ys)
let insert (xs, ys) x = (xs, x :: ys)

let rec insertall' x (xs, ys) =
  match xs with
  | [] -> [list_of_ziplist ([], x :: ys)]
  | xs -> (list_of_ziplist (xs, x :: ys)) :: (insertall' x (left (xs, ys)))

let insertall x xs = insertall' x (ziplist_of_list xs) 

let rec insert_perm xs =
  match xs with
  | [] -> [[]]
  | x :: xs -> List.flatten (List.map (insertall x) (insert_perm xs))


let rec chooseall' (xs, ys) = 
  match xs with
  | [] -> []
  | x :: xs -> (x , list_of_ziplist (xs, ys)) :: chooseall' (left (x :: xs, ys))

let chooseall xs = chooseall' (xs, [])

let rec choose_perm xs = 
  match xs with
  | [] -> [[]]
  | xs -> List.flatten (List.map (fun (a, al) -> List.map (List.cons a) (choose_perm al)) (chooseall xs))