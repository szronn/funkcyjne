

let length = List.fold_left (fun acc x -> acc + 1) 0
let rev = List.fold_left (fun acc x -> x :: acc) []
let filter pred = List.fold_right (fun x acc -> if pred x
                                                then x :: acc
                                                else acc)
                                  []
let rev_map f = List.fold_left (fun acc x -> f x :: acc) []
let map f = List.fold_right (fun x acc -> f x :: acc) []
let append xs = List.fold_right List.cons xs
let rev_append xs = List.fold_left (fun acc x -> x :: acc) (rev xs)

 