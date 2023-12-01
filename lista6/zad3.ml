exception Nie 

let for_all p xs =
  try
    List.fold_left
      (fun acc x -> if not (p x)
                    then raise Nie
                    else acc && (p x))
      true
      xs
  with Nie -> false


let mult_list xs =
  try
    List.fold_left
      (fun acc x -> if x = 0
                    then raise Nie
                    else acc * x)
      1
      xs
  with Nie -> 0

let sorted xs = 
  try let _ = (List.fold_left
    (fun prev x -> if prev > x
                      then raise Nie
                      else x)
    0 (*dużo*)
    xs) in true
  with Nie -> false
 

                    

