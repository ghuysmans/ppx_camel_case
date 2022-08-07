type v =
  | First_order [@name "fo"]
  | Hard of {k: [`A of [`X] [@name "alpha"]]}
  [@@camel_case] [@@deriving yojson]

type t = {
  one: int;
  fourty_two: int;
  dont_touch: v list [@key "don_t_touch"];
} [@@camel_case] [@@deriving yojson]

let () =
  let t = {one = 1; fourty_two = 42; dont_touch = [First_order; Hard {k = `A `X}]} in
  let expected = {|{"one":1,"fourtyTwo":42,"don_t_touch":[["fo"],["hard",{"k":["alpha",["x"]]}]]}|} in
  let actual = Yojson.Safe.to_string (to_yojson t) in
  assert (actual = expected)
