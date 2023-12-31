module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Id: Monad = struct
  
  type 'a t = 'a 

  let return x = x 

  let bind m f = f m 

  let run m = m
  
end

module Lazy: Monad = struct
  type 'a t = unit -> 'a 

  let return x = fun () -> x 

  let bind m f =
    fun () ->
      (f (m ()) ())

  let run m = m ()

end
