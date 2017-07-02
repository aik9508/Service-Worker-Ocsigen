[%%client.start]
open ServiceWorker

let fetch (request:_request Js.t) : _response Js.t =
  let f = Js.Unsafe.global##.fetch in
  Js.Unsafe.fun_call f [|Js.Unsafe.inject request|]