
let foo a b = a + b [@@inline never]

let bar i = foo (foo i i) i [@@inline always]

let foobar i = bar (bar i)

