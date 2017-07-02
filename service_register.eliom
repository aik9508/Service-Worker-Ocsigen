[%%client.start]
open Js
open Dom_html
open Promise

type service_worker

let serviceWorker = Unsafe.get Unsafe.global##.navigator "serviceWorker"

let register ?(options: 'a option) (scriptURL:string) : service_worker promise t =
  match options with
  | None ->
    Unsafe.meth_call serviceWorker "register" [|Unsafe.inject scriptURL|]
  | Some options ->
    Unsafe.meth_call serviceWorker "register" [|Unsafe.inject scriptURL; Unsafe.inject options|]

(*let () =
  let p = register "service_worker.js" in
  ignore @@
  _then p
    ~catch:(fun _ -> Firebug.console##log (string "cannot be registered"))
    (fun _ -> Firebug.console##log (string "registered"))*)

