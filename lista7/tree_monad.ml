type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list

return x = (Var(x))

let rec bind m f = 
match m with
| Var(v) -> f v
| Sym(s, xs) -> map (fun x -> bind x f) xs