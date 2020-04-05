
open Bigarray

let () =
  let a = Array1.create int c_layout 3 in
  for i = 0 to 2 do a.{i} <- i done;
  try
    let _ = a.{3} in
    print_int 42
  with Invalid_argument _ ->
    print_int 113

