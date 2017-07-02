[%%client.start]
open Js
open Dom_html
open Promise
open ServiceWorker

type cacheName = js_string t

class type cache = object
  (*method _match : _request t -> _response promise t meth
  method _match_ : _request t -> 'a -> _response promise t meth*)
  method addAll : js_string t js_array t -> unit meth
  method put : _request t -> _response t -> unit promise t
end

let caches = Unsafe.global##.caches

let _open (cacheName:cacheName) : cache t promise t =
  Unsafe.meth_call caches "open" [|Unsafe.inject cacheName|]

let delete (cacheName:cacheName) : unit promise t =
  Unsafe.meth_call caches "delete" [|Unsafe.inject cacheName|]

let keys ?(request: _request option) ?(options: 'a option) () 
  : cacheName js_array t promise t = 
  match (request,options) with
  | (None, None) -> Unsafe.meth_call caches "keys" [||]
  | (Some request, None) -> 
      Unsafe.meth_call caches "keys" [|Unsafe.inject request|]
  | (None, Some options) -> 
      Unsafe.meth_call caches "keys" [|Unsafe.inject options|]
  | (Some request, Some options) ->
      Unsafe.meth_call caches "keys" [|Unsafe.inject request; Unsafe.inject options|]

let _match (request:_request t) : _response t opt promise t =
  Unsafe.meth_call caches "match" [|Unsafe.inject request|]

let _match2 (url : js_string t) : _response t opt promise t =
  Unsafe.meth_call caches "match" [|Unsafe.inject url|]
