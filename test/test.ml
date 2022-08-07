type t = {
  one: int;
  fourty_two: int;
  dont_touch: string [@key "don_t_touch"];
} [@@camel_case] [@@deriving yojson]

let () =
  let t = {one = 1; fourty_two = 42; dont_touch = "!"} in
  let expected = {|{"one":1,"fourtyTwo":42,"don_t_touch":"!"}|} in
  assert (Yojson.Safe.to_string (to_yojson t) = expected)
