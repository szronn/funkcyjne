type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
  { prev : 'a dllist;
  elem : 'a;
  next : 'a dllist 
  } 

let prev d = (Lazy.force d).prev
let elem d = (Lazy.force d).elem
let next d = (Lazy.force d).next


let rec helper xs prev first = 
  match xs with
  | [] -> (first, prev)
  | x :: xs -> let rec curr = lazy begin
                let (next, last) = helper xs (lazy (fst (Lazy.force curr))) first in
                ({prev = prev; elem = x; next = next}, last)
                end in
                let res = (fst (Lazy.force curr)) in
                let lst = (snd (Lazy.force curr)) in
                (lazy res, lst)

let rec of_list xs = 
  match xs with
  | [] -> failwith "nie chce mi sie tego rozpatrywac"
  | x :: xs -> let rec first = lazy begin
    let (next, last) = helper xs first first in
    { prev = last; elem = x; next = next }
    end in first


let rec go_right n prev =
  let rec curr = lazy begin
    let next = go_right (n + 1) curr in
    {prev = prev; elem = n; next = next}
  end in curr

let rec go_left n next = 
  let rec curr = lazy begin
    let prev = go_left (n - 1) curr in
    {prev = prev; elem = n; next = next}
  end in curr 


let integers = 
  let rec curr = lazy begin
    let left = (go_left (-1) curr) in
    let right = (go_right 1 curr) in
    {prev = left; elem = 0; next = right}
  end in curr 


  let () = assert((prev (next integers)) == integers);;
  let () = assert((next (prev (next integers))) == (next integers));;
  let () = assert((prev (next (next integers))) == (next integers));;
  let () = assert((next (next (prev (next (next integers))))) == (next (next (next integers))));;
