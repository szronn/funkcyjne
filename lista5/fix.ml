
let rec fix f x = f (fix f) x

let fib_f fib n = 
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let fib = fix fib_f

let rec fix_with_limit n f x = 
  if n = 0 then failwith "max recursion depth reached"
  else f (fix_with_limit (n - 1) f) x

let fib_with_limit n = fix_with_limit n fib_f

let fix_memo f x = 
  let table = Hashtbl.create 1 in
  let rec inner f x = match Hashtbl.find_opt table x with
                      | Some y -> y
                      | None -> let res = f (inner f) x in
                                Hashtbl.add table x res;
                                res
  in
  inner f x

(* fib 40 -> 12 sekund, fib_memo 40 -> instant
   fib_with_limit 3 40 -> wypierdala sie na pysk *)
let fib_memo = fix_memo fib_f


let fix_mut f x = 
  let box = ref (fun _ -> failwith "auc") in
  let g a = !box a in
  let f = f g in
  box := f;
  f x

let fib_mut = fix_mut fib_f

type 'a self = Fold of  ('a self -> 'a)

let unfold (Fold t) = t

(* trzeba używać rozsz rectypes *)
let fix_proper f x = (fun g a -> f ((unfold g) g) a) (Fold(fun g a -> f ((unfold g) g) a)) x

let fib_proper = fix_proper fib_f

let () = print_endline (string_of_int (fib_proper 6))








