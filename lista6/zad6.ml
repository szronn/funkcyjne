
let rec echo k =
recv (fun v ->
send v (fun () ->
echo k))

let rec map f k =
  recv (fun v ->
  send (f v) (fun () ->
  map f k))

let rec filter p k = 
  recv (fun v ->
  if (p v)
  then send v (fun () -> filter p k)
  else filter p k)

let rec nats_from n k =
  send n (fun () -> nats_from (n + 1) k)


let rec sieve k =
  recv (fun v ->
  send v (fun () ->
  (filter (fun k -> k mod v <> 0) >|> sieve) k))

 