module BT : sig
  type 'a t = 'a Seq.t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Brak wyniku *)
  val fail : 'a t
  (** Niedeterministyczny wybór -- zwraca true, a potem false *)
  val flip : bool t

  val run : 'a t -> 'a Seq.t
  end = struct
  (* Obliczenie typu 'a to leniwa lista wszystkich możliwych wyników *)
  type 'a t = 'a Seq.t

  let return x = List.to_seq [ x ]
  let rec bind m f = Seq.flat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

type empty = |

type _ fin_type = 
  | Unit  : unit fin_type
  | Bool  : bool fin_type
  | Pair  : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
  | Left  : 'a fin_type               -> ('a, empty) Either.t fin_type
  | Right : 'b fin_type               -> (empty, 'b) Either.t fin_type
  | Either: 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type
  | Empty : empty fin_type 

let (let* ) = BT.bind

let rec all_values : type a. a fin_type -> a Seq.t = 
  fun typ ->
    match typ with
    | Unit         -> BT.return ()
    | Bool         -> let* c = BT.flip in BT.return c
    | Pair(a, b)   -> let* a = all_values a in 
                    let* b = all_values b in
                    BT.return (a , b)
    | Left(a)      -> let* a = all_values a in
                    BT.return (Either.Left(a))
    | Right(b)     -> let* b = all_values b in 
                    BT.return (Either.Right(b))
    | Either(a, b) -> let* c = BT.flip in
                      if c then let* a = all_values a in BT.return (Either.left(a))
                      else let* b = all_values b in BT.return (Either.Right(b)) 
    | Empty -> Seq.empty  
    