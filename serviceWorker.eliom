[%%client.start]
open Js
open Dom_html
open Promise

class type installEvent = object 
  inherit event
  method waitUntil: unit promise t -> unit meth
end

class type _request = object
   method url: js_string t readonly_prop
   method _Method: js_string t readonly_prop
end

class type _response = object 
  method clone : unit -> _response t meth
  method json : unit -> _response t promise t meth
end

class type fetchEvent = object
  inherit event

  method respondWith : _response t promise t -> unit meth
  method default: _request t -> 'a meth
  method request: _request t readonly_prop
end

let addEvent e h =
  addEventListener window e h _false

let addInstallListener handler : event_listener_id =
  addEvent (Event.make "install" : installEvent t Event.typ) handler

let addActivateListener handler : event_listener_id =
  addEvent (Event.make "activate" : installEvent t Event.typ) handler

let addFetchListener handler : event_listener_id =
  addEvent (Event.make "fetch" : fetchEvent t Event.typ) handler

(*let _Response = Unsafe.global##._Response*)