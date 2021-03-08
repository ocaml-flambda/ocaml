
type t =
  | Foo1 of { foo : int; }
  | Foo2 of { foo : int; bar : float; }
  | Foo3 of { foo : int; foobar : string; }

let foo b x y =
  let f =
    if b
    then Foo1 { foo = x; }
    else Foo2 { foo = x; bar = y +. 1.; }
  in
  match f with
  | Foo1 { foo; } -> foo
  | Foo2 { foo; bar; } -> foo + (int_of_float bar)
  | Foo3 _ -> assert false

