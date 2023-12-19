module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig
  include Monad
  val fail : 'a t
  val catch : 'a t -> (unit -> 'a t) -> 'a t
  val run : 'a t -> 'a option
end = struct
  type 'a t = { run : 'r. ('a -> 'r option) -> 'r option }

  let return x = {run = fun cnt -> cnt x }

  let bind m f = {run = fun cnt -> 
    m.run (fun x ->
      (f x).run (fun y ->
        cnt y))}
  (* let bind m f = {run = fun cnt ->
    match m.run (fun x -> Some(x)) with
    | None -> None
    | Some(v) -> (f v).run cnt } *)

  let fail = {run = fun cnt -> None }

  let catch (a : 'a t) (f : unit -> 'a t) : 'a t = {run = (fun cnt ->
    match a.run cnt with
    | Some(v) -> Some(v)
    | None -> (f ()).run cnt
  )}
  
  let run a = a.run (fun x -> Some(x))

end

module BT : sig
  include Monad
  val fail : 'a t
  val flip : bool t
  val run : 'a t -> 'a Seq.t
end = struct

  type 'a t = { run : 'r. ('a -> 'r Seq.t) -> 'r Seq.t }

  let return x = {run = fun cnt -> cnt x}
  let bind m f = {run = fun cnt -> 
    m.run (fun x ->
      (f x).run (fun y ->
        cnt y))}

  let fail = {run = fun cnt -> Seq.empty}
  
  let flip = {run = fun cnt -> (Seq.append (cnt true) (cnt false))}

  let run a = a.run Seq.return 

end

module St(State : sig type t end) : sig
  include Monad
  val get : State.t t
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a
end = struct
  type 'a t = { run : 'r. ('a -> (State.t -> ('r * State.t))) -> (State.t -> ('r * State.t))}

  let return x = {run = fun cnt -> cnt x}

  let bind m f = {run = fun cnt -> 
    m.run (fun x ->
      (f x).run (fun y ->
        cnt y))}

  let get = {run = fun cnt s -> cnt s s}
  let set s = {run = fun cnt -> (fun z -> cnt () s)}
  let run s a = fst (a.run (fun x -> (fun z -> (x, z))) s)
end


  (* let (let* ) = BT.bind



  let z = let* coin = BT.flip in
    if coin then (BT.return 1)
            else let* coin = BT.flip in 
                  if coin then (BT.return 2)
                  else (BT.return 3)  *)
  
  module Cnt = St(Int)
  let (let*) = Cnt.bind

  let counter = let* s = Cnt.get in
      print_int s; let* _ = Cnt.set (s + 1) in
                  let* s2 = Cnt.get in
      print_int s2; Cnt.return "ez"

    