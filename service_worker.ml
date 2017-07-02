let cacheName = Js.string "demo-ocsigen-sw-1.0"
let dataCacheName = Js.string "weatherData-v1"

let filesToCache = [|
  "/";
  "/css/pwaeliom.css";
  "/pwaeliom.js";
  "/images/nanjing1.jpg";
|] 
  |> Array.map Js.string
  |> Js.array

let install_handler = 
  Dom_html.handler (fun ev -> 
      Firebug.console##log 
        (Js.string "[ServiceWorker] Installed");
      ev##waitUntil (
        let p = Cache._open cacheName in
        Promise._then p 
          (fun cache -> 
             Firebug.console##log 
               (Js.string  "[ServiceWorker] Catch app shell");
             cache##addAll filesToCache)
      );
      Js._false)

let activate_handler = 
  Dom_html.handler (fun ev -> 
      Firebug.console##log
        (Js.string "[ServiceWorker] Activate");
      ev##waitUntil (
        let keyList = Cache.keys () in
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
                   Cache.delete key
                 end 
                 else Promise.resolve_value ()
               )
             |> Js.array
             |> Promise.all));
      Clients.claim ();
      Js._false)

let fetch_handler =
  Dom_html.handler (fun ev ->
      Firebug.console##log 
        ((Js.string "[ServiceWorker] Fetch ")##concat (ev##.request##.url));
      let dataUrl = "https://query.yahooapis.com/v1/public/yql" in
      if Regexp.string_match (Regexp.regexp_string dataUrl) (Js.to_string ev##.request##.url) 0 <> None 
      then begin
        ev##respondWith(
          let p = Cache._open dataCacheName in
          Promise._then p
            (fun cache -> 
               Promise._then 
                 (Sw_window.fetch ev##.request)
                 (fun res -> 
                    ignore (cache##put (Js.Unsafe.coerce ev##.request##.url) (res##clone ()));
                    Promise.to_promise res)))
      end
      else
        ev##respondWith(
          Promise._then 
            (Cache._match ev##.request)
            (fun response -> 
               Promise.to_promise (
                 Js.Opt.get response 
                   (fun () -> Js.Unsafe.coerce (Sw_window.fetch ev##.request)))));
      Js._false)

let () = 
  ignore @@ ServiceWorker.addInstallListener install_handler ;
  ignore @@ ServiceWorker.addActivateListener activate_handler;
  ignore @@ ServiceWorker.addFetchListener fetch_handler;
