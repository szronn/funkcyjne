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

let (let*) = BT.bind

type 'a regexp =
| Eps
| Lit of ('a -> bool)
| Or of 'a regexp * 'a regexp
| Cat of 'a regexp * 'a regexp
| Star of 'a regexp


let ( +% ) r1 r2 = Or(r1, r2)
let ( *% ) r1 r2 = Cat(r1, r2)



(* let rec matches reg xs = 
  match reg with
  | Eps -> false
  | Lit(f) -> begin match xs with
              | x :: [] -> f x
              | xs -> false
              end
  | Or(reg1, reg2) -> (matches reg1 xs) || (matches reg2 xs)
  | Cat(reg1, reg2) ->    

   *)


let rec match_regexp reg xs =
  match reg with
  | Eps -> BT.return None
  | Lit(f) -> begin match xs with
              | x :: xs -> if f x then BT.return (Some(xs))
                          else BT.fail
              | [] -> BT.fail
              end
  | Or(reg1, reg2) -> let* coin = BT.flip in 
                      (match_regexp (if coin then reg1
                                    else reg2) xs)
                       
  | Cat(reg1, reg2) -> let* mat1 = match_regexp reg1 xs in 
                      begin match mat1 with
                      | None -> match_regexp reg2 xs
                      | Some(ys) -> let* mat2 = match_regexp reg2 ys in
                                    begin match mat2 with
                                    | None -> BT.return (Some(ys))
                                    | z -> BT.return z
                                    end
                      end

    (* let* a = f x in e (to samo co) bind f x (fun a -> e)  *)

  | Star(reg) -> 
    let* coin = BT.flip in 
    if coin then BT.return None
    else
    let* mat = match_regexp reg xs in
                  match mat with
                  | None -> BT.return None
                  | Some(xs) -> let* mat2 = match_regexp (Star(reg)) xs in
                                begin match mat2 with
                                | None -> BT.return (Some(xs))
                                | z -> BT.return z
                                end


let pat  = Star (Star (Lit ((<>) 'b')) +% (Lit ((=) 'b') *% Lit ((=) 'a')))

let test1 = match_regexp pat ['c';'b';'a']
let suf = match_regexp pat ['z';'b';'c';'b';'a';'c';'b';'a';'a'];;

      
                      

