

type ('a, 'b) format = (string -> 'b) -> string -> 'a



let lit z cnt = fun s -> cnt (s ^ z)    


let str cnt s z = 
  cnt (s ^ z)

let int cnt s n = 
  cnt (s ^ (string_of_int n))


let (^^) f1 f2 = 
  fun f -> f1 (f2 f)


let ksprintf (f : ('a, 'b) format) cnt = (f cnt) ""

let id x = x

let sprintf f = (ksprintf f) id

(* let () = ksprintf (lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit ".") *)
