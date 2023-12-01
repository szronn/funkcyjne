
type ('x, 'y) format =
  | Int : ((int -> 'c), 'c) format
  | Str : (string -> 'c, 'c) format
  | Lit : string -> ('c, 'c) format
  | Cat : ('a, 'b) format * ('b, 'c) format -> ('a, 'c) format


let rec ksprintf : type a b. (a, b) format -> (string -> b) -> a = 
  fun f cnt -> 
    match f with
    | Int         -> (fun n -> cnt (string_of_int n))
    | Str         -> (fun z -> cnt z)
    | Lit(s)      -> cnt s
    | Cat(f1, f2) -> ksprintf f1 (fun s -> ksprintf f2 (fun z -> cnt (s ^ z)))
    
let id x = x
let sprintf f = ksprintf f id  


let rec kprintf : type a b. (a, b) format -> b -> a =
  fun f bb ->
    match f with
    | Int         -> (fun n -> print_int n; bb)
    | Str         -> (fun s -> print_string s; bb)
    | Lit(s)      -> print_string s ; bb
    | Cat(f1, f2) -> kprintf f1 (kprintf f2 bb)

let printf f = kprintf f ()



