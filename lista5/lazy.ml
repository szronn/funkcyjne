type 'a inner =
  | Delayed of (unit -> 'a)
  | Done of 'a
  | Evaluating of (unit -> 'a)

type 'a my_lazy = 'a inner ref

let force x = match !x with
  | Delayed f -> let v = f () in x :=  Done v
  | Done _ -> ()
  | Evaluating _ -> failwith "tak nie wolno nie wolno tak"



let mlazy f = (ref (Delayed(f)))

let value x = 
  (force x);
  match !x with
  | Done v -> v
  | _ -> failwith "hehe"

let change_to_eval l = 
  match !l with
  | Delayed f -> l := Evaluating f
  | _ -> ()

let change_back l = 
  match !l with
  | Evaluating f -> l := Delayed f
  | _ -> ()


let rec fix f = ref(Delayed((fun _ ->
  let v = fix f in
  change_to_eval v;
  let res = f v in
  change_back v;
  res)))

(* let rec fix f = ref (Delayed((fun _ -> (f (change_to_eval (fix f)))))) *)

(* let rec fix f = ref Delayed(fun _ -> f (ref (Evaluating (fun _ -> (change_to_eval (fix f)))))) *)


type colist =
  | Nil
  | Cons of (int * colist my_lazy) 


let stream_of_ones = fix (fun stream_of_ones -> Cons(1, stream_of_ones))


let rec ad_infinitum cl = 
  match value cl with
  | Cons(n, cl) -> print_endline (string_of_int n); ad_infinitum cl
  | _ -> failwith "hehe"



let rec filter cl p =
  match cl with
  | Cons(n, cl) -> if p n then Cons(n, (mlazy (fun () -> (filter (value cl) p))))
                          else filter (value cl) p
  | Nil -> Nil  

let rec map cl f =
  match cl with
  | Cons(n, cl) -> Cons((f n), (mlazy (fun () -> (map (value cl) f))))
  | Nil -> Nil


let rec take_while cl p = 
  match cl with
  | Cons(n, cl) -> if p n then Cons(n, (mlazy (fun () -> (take_while (value cl) p))))
                          else Nil
  | Nil -> Nil 

let rec any cl p = 
  match cl with
  | Cons(n, cl) -> if p n then true
                          else (any (value cl) p)
  | Nil -> false 


(* let rec nats_from n = fix (fun nats -> Cons(n, (map nats ((+) 1)))) *)

let rec nats_from n = Cons(n, (ref (Delayed((fun _ -> (nats_from (n + 1)))))))



let primes = (fix (fun primes -> Cons(2,(mlazy (fun () ->
  let is_prime n = 
    (not (any
    (take_while (value primes) (fun p -> (p * p <= n)))
    (fun p -> n mod p = 0)))
  in
  (filter (nats_from 3) is_prime)))
)))
