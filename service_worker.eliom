[%%client.start]
open ServiceWorker

let cacheName = Js.string "demo-ocsigen-sw-1.0"
let dataCacheName = Js.string "weatherData-v1"

let filesToCache = [|
  "/";
  "/css/pwaeliom.css";
  "/pwaeliom.js";
  "/images/nanjing1.jpg";
  "/images/icons/icon-32x32.png";
  "/images/icons/icon-128x128.png";
  "/images/icons/icon-144x144.png";
  "/images/icons/icon-152x152.png";
  "/images/icons/icon-192x192.png";
  "/images/icons/icon-256x256.png"
|] 
  |> Array.map Js.string
  |> Js.array

let install_handler () = 
  let self = get_self () in
  Dom_html.handler (fun (ev:installEvent Js.t) -> 
      Firebug.console##log 
        (Js.string "[ServiceWorker] Installed");
      ev##waitUntil (
        let p = self##.caches##_open cacheName in
        Promise._then p 
          (fun cache -> 
             Firebug.console##log (Js.string  "[ServiceWorker] Catch app shell");
             cache##addAll_withUrl filesToCache)
      );
      Js._false)

let activate_handler () = 
  let self = get_self () in
  Dom_html.handler (fun (ev:activateEvent Js.t) -> 
      Firebug.console##log (Js.string "[ServiceWorker] Activate");
      ev##waitUntil (
        let keyList = self##.caches##keys in
        Promise._then keyList
          (fun keyList -> 
             keyList 
             |> Js.to_array
             |> Array.map (fun key ->
                 if key <> cacheName && key <> dataCacheName
                 then begin
                   Firebug.console##log (
                     Js.string ("[ServiceWorker] Removing old cache" ^
                                (Js.to_string key)));
                   self##.caches##delete key
                 end 
                 else Promise.resolve_value Js._false
               )
             |> Js.array
             |> Promise.all));
      ignore @@ self##.clients##claim;
      Js._false)

let fetch_handler () =
  let self = get_self () in
  Dom_html.handler (fun (ev:fetchEvent Js.t) ->
      Firebug.console##log 
        ((Js.string "[ServiceWorker] Fetch ")##concat (ev##.request##.url));
      let dataUrl = "https://query.yahooapis.com/v1/public/yql" in
      if Regexp.string_match 
          (Regexp.regexp_string dataUrl) 
          (Js.to_string ev##.request##.url) 0 <> None 
      then begin
        ev##respondWith(
          let p = self##.caches##_open dataCacheName in
          Promise._then p
            (fun cache -> 
               Promise._then 
                 (self##fetch ev##.request)
                 (fun res -> 
                    ignore (cache##put_withUrl ev##.request##.url (res##clone));
                    Promise.resolve_value res)))
      end
      else
        ev##respondWith(
          Promise._then 
            (self##.caches##_match ev##.request)
            (fun response -> 
               Promise.resolve_value (
                 Js.Opt.get response 
                   (fun () -> Js.Unsafe.coerce (self##fetch ev##.request)))));
      Js._false)

let () = 
  ignore @@ ServiceWorker.addInstallListener (install_handler ()) ;
  ignore @@ ServiceWorker.addActivateListener (activate_handler ());
  ignore @@ ServiceWorker.addFetchListener (fetch_handler());