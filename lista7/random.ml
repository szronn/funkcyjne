module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t

  val run_random : int -> 'a t -> 'a 
end

module RS: RandomMonad = struct
  type 'a t = int -> ('a * int)

  let gen_new a =
    let b = 16807 * (a mod 127773) - 2836 * (a / 127773) in
    if b > 0 
    then b
    else b + 2147483647

  let return x = fun a ->
  let a = gen_new a
  in (x, a) 

  let bind m f =
    fun x ->
      let (v, rand) = m x
      in (f v) rand

  let random = fun a ->
    let n = gen_new a
    in (a, n)
  
  let run_random seed m = fst (m seed)
end

module Shuffle(R: RandomMonad): sig
  val shuffle : 'a list -> 'a list R.t
end = struct
  let (let* ) = R.bind

  let rec shuffle xs = 
    let* rand = R.random in
    match xs with
    | x :: y :: xs -> if (rand mod 2) = 0
                      then let* rest = shuffle (y :: xs) in R.return (x :: rest)
                      else let* rest = shuffle (x :: xs) in R.return (y :: rest)
    | xs -> R.return xs  
end

module Shuf = Shuffle(RS)

