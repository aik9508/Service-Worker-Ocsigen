[%%client.start]
let clients = Js.Unsafe.global##.clients

let claim () : unit =
  Js.Unsafe.meth_call clients "claim" [||]
