
(*zad 3*)
let hd s = s 0
let tl s = fun x -> s (x + 1)
let add s n = fun x -> (s x) + n
let map s f = fun x -> f (s x)
let map2 f s1 s2 = fun x -> f (s1 x) (s2 x)
let replace n a s = fun x -> if x = n then a else s x
let take_every n s = fun x -> s (n * x)

let cons s ss = fun x -> if x = 0 then s else ss (x - 1) 
let back n ss = fun x -> ss (x - n)


let num_stream n : int = n
let const_stream x = fun n : int -> x

                        
let rec fib_stream x = cons 0 (cons 1 (map2 (+) (tl fib_stream) fib_stream)) x


(*zad 4*)
let rec scan f s v = fun x -> if x = 0 
                              then f (s 0) v
                              else f (s x) ((scan f s v) (x - 1)) 

let num_stream2 = scan (+) (const_stream 1) 0

let tln s n = fun x -> s (x + n)

(*zad 6*)
(*w zadaniu nie jest napisane w jakiej maja byc kolejnosci :p (są w odwrotnej) *)
let tabulate s ?(from = 0) until = (scan List.cons (tln s from) []) (until - from)


                           