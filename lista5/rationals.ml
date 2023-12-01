
type rat = Rat of (rat Lazy.t) * (int * int) * (rat Lazy.t)

let rec between (a, b) (c, d) = Rat((lazy (between (a, b) (a + c, b + d))),
                              (a + c, b + d),
                              (lazy (between (a + c, b + d) (c, d))))


let rationals = between (0, 1) (1, 0)
