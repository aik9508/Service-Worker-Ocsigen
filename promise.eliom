[%%client.start]
type 'a promise

let _Promise = Js.Unsafe.global##._Promise

let _catch (p : 'a promise Js.t) (f : _ -> 'a promise Js.t) : 'a promise Js.t =
  (Js.Unsafe.coerce p)##_catch (Js.wrap_callback f)

let _then (p : 'a promise Js.t) ?(catch: (_ -> 'b promise Js.t) option)
    (f: 'a -> 'b promise Js.t) : 'b promise Js.t =
  match catch with
  | None ->
      (Js.Unsafe.coerce p)##_then (Js.wrap_callback f)
  | Some catch ->
      (Js.Unsafe.coerce p)##_then (Js.wrap_callback f) (Js.wrap_callback catch)

let resolve_value (v:'a) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "resolve" [|Js.Unsafe.inject v|]

let resolve_promise (v:'a promise Js.t) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "resolve" [|Js.Unsafe.inject v|]

let to_promise (v:'a Js.t) : 'a Js.t promise Js.t =
  Js.Unsafe.coerce v

let all (promise_list : 'a promise Js.t Js.js_array Js.t) : 'a promise Js.t =
  Js.Unsafe.meth_call _Promise "all" [|Js.Unsafe.inject promise_list|]

let to_lwt (p : 'a promise Js.t) : 'a Lwt.t =
  let (r, w) = Lwt.task () in
  ignore
    ((Js.Unsafe.coerce p)##_then
      (Js.wrap_callback (fun v -> Lwt.wakeup w v))
      (Js.wrap_callback (fun e -> Lwt.wakeup_exn w (try raise e with e -> e))));
  r

let of_lwt (p : unit -> 'a Lwt.t) : 'a promise Js.t =
  new%js _Promise
    (Js.wrap_callback
       (fun resolve reject ->
          Lwt.try_bind p
            (fun v -> Js.Unsafe.fun_call resolve [|Js.Unsafe.inject v|])
            (fun e -> Js.Unsafe.fun_call reject [|Js.Unsafe.inject e|])))
