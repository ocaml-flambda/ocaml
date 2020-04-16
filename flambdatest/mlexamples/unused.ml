
let foo a b = a + b [@@inline never][@@local never]

let bar i = foo i i [@@inline always][@@local never]

let foobar i = bar i

