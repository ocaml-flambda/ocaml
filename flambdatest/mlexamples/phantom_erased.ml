
let id x _ = x

let[@inline] f b x =
  let r = ref x in
  if b then begin
    incr r;
    !r
  end else begin
    let n = !r + 1 in
    id 42 n
  end

let g x =
  f false x

