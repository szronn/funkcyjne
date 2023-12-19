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
  type 'a t = State.t -> ('a * State.t) Seq.t

  let return x = (fun s -> (List.to_seq [(x, s)]))

  
  let bind (m : 'a t) (f : 'a -> 'b t) = fun s ->
    let xs = m s in 
    Seq.flat_map (fun (a, ns) -> ((f a) ns)) xs
  
  let fail = fun s -> Seq.empty

  let flip = fun s -> (List.to_seq [(true, s);(false, s)])

  let get = fun s -> List.to_seq [(s, s)]

  let put s = fun z -> List.to_seq [((), s)]

  let run (s : State.t) (m : 'a t) = Seq.map fst (m s)
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
  else Cnt.get 