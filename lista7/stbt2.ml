module SBT(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
end = struct
  (* polesiuk sie pomylil ugulem *)
  type 'a t = State.t -> 'a sbt_list
  and 'a sbt_list =
  | Nil of State.t
  | Cons of 'a * State.t * 'a t

  let fail = fun s -> Nil(s)
  let return x = fun s -> (Cons(x, s, fail))

  
  (* let rec append m n =
    match m with
    | Nil -> failwith "eee"
    | Cons(v, s, r) -> Cons(v, s, fun s -> begin match (r s) with
                                           | Nil -> n s
                                           | _ -> append (r s) n
                                            end) *)

  let rec append m n s = 
    match m s with
    | Nil(s) -> n s
    | Cons(v, s, r) -> Cons(v, s, append r n)

  (* let rec bind m f = fun s ->
    let xs = m s in
    match xs with
    | Nil -> Nil
    | Cons(v, ns, m) -> append ((f v) ns) (bind m f)
   *)
  let rec bind m f = fun s ->
    let xs = m s in 
    match xs with
    | Nil(s) -> Nil(s)
    | Cons(v, ns, n) -> append (f v) (bind n f) ns

  

  let flip = fun s -> Cons(true,s, fun z -> Cons(false, z, fail))
  let get = fun s -> Cons(s, s, fail)

  let put s = fun z -> Cons((), s, fail)

  let rec run s m =
    match m s with
    | Nil(_) -> Seq.empty
    | Cons(v, s, m) -> Seq.cons v (run s m) 
end

module Cnt = SBT(Int)
let (let*) = Cnt.bind

let rec count_even n curr = 
  if curr > n then Cnt.get
  else let* coin = Cnt.flip in
  if coin 
    then let* acc = Cnt.get in
         let* _ = Cnt.put (acc + 1) in
         count_even n (curr + 1)
    else count_even n (curr + 1)

let ass = let* coin = Cnt.flip in 
    if coin
    then let* acc = Cnt.get in
         let* _ = Cnt.put (acc + 1) in 
         Cnt.get
    else let* coin = Cnt.flip in
        if coin
        then let* acc = Cnt.get in
             let* _ = Cnt.put (acc + 1) in 
             Cnt.get
        else Cnt.get  


let m = let* _ = Cnt.put 13 in 
let* coin = Cnt.flip in 
if coin then let* _ = Cnt.put 42 in Cnt.fail
        else Cnt.get 