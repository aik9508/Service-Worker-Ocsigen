[%%shared
  open Eliom_lib
  open Eliom_content
  open Html.D
  open Lwt
]

module Pwaeliom_app =
  Eliom_registration.App (
  struct
    let application_name = "pwaeliom"
    let global_data_path = None
  end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

[%%shared
  type card = {
    c_city : Html_types.div Eliom_content.Html.D.elt;
    c_humidity: Html_types.div Eliom_content.Html.D.elt;
    c_pressure: Html_types.div Eliom_content.Html.D.elt;
    c_lastBuildDate: Html_types.div Eliom_content.Html.D.elt;
  }

  let create_card () ={
    c_city = div [];
    c_humidity= div [];
    c_pressure = div [];
    c_lastBuildDate = div []}

  let wrap_card c = 
    div ~a:[a_class ["pwa-weather-bord"]] 
      [c.c_city; c.c_lastBuildDate; c.c_humidity; c.c_pressure;]

  let cards : (string * card) list ref = ref []
]

[%%client

  type forecast_results =
    {city: Js.js_string Js.t; 
     humidity: Js.js_string Js.t; 
     pressure: Js.js_string Js.t;
     lastBuildDate: Js.js_string Js.t}

  let json : < parse : Js.js_string Js.t -> 'a> Js.t = Js.Unsafe.pure_js_expr "JSON"

  let json_get json attr_list =
    List.fold_left (fun json attr -> Js.Unsafe.get json attr) json attr_list

  let to_results data =
    let channel = json_get data ["query";"results";"channel"] in
    let city = json_get channel ["location";"city"] in
    let humidity = json_get channel ["atmosphere"; "humidity"] in
    let pressure = json_get channel ["atmosphere"; "pressure"] in
    let lastBuildDate = json_get channel ["lastBuildDate"] in
    {city; humidity; pressure; lastBuildDate}

  let http_get url =
    XmlHttpRequest.get url >>= fun r ->
    let cod = r.XmlHttpRequest.code in
    let msg = r.XmlHttpRequest.content in
    (*Firebug.console##log (Js.string msg) ;*)
    if cod = 0 || cod = 200
    then Lwt.return msg
    else fst (Lwt.wait ())

  let updateForecastCard (card:card) (results:forecast_results) =
    (Html.To_dom.of_element card.c_city)##.innerHTML :=
      (Js.string "City : ")##concat results.city ;
    (Html.To_dom.of_element card.c_lastBuildDate)##.innerHTML :=
      (Js.string "Last build date : ")##concat results.lastBuildDate;
    (Html.To_dom.of_element card.c_humidity)##.innerHTML :=
      (Js.string "Humidity : ")##concat results.humidity;
    (Html.To_dom.of_element card.c_pressure)##.innerHTML :=
      (Js.string "Pressure : ")##concat results.pressure

  let getForecast key card = 
    let statement = "select * from weather.forecast where woeid=" in
    let url = "https://query.yahooapis.com/v1/public/yql?format=json&q=" 
              ^ statement ^ key in
    ignore( 
      try
        let self = ServiceWorker.create_self () in
        let p_res = self##.caches##match_withUrl (Js.string url) in
        Promise._then p_res 
          (fun res -> 
             Js.Opt.iter res 
               (fun res -> 
                  ignore @@
                  Promise._then 
                    (res##json)
                    (fun res ->
                       Firebug.console##log (Js.string "Fetch succeeds..");
                       Firebug.console##log (Js.Unsafe.coerce res);
                       updateForecastCard card (to_results (Js.Unsafe.coerce res));
                       Promise.resolve_value ()));
             Promise.resolve_value ())
      with _ ->
        Promise.resolve_value (Firebug.console##log (Js.string "Fetch fails .."))); 
    http_get url >>= 
    (fun s -> 
       updateForecastCard card (to_results (json##parse (Js.string s)));
       Lwt.return_unit
    )

  (*let initialWeatherData = 
    {c_city = div [pcdata "City : New York"];
     c_humidity =div [pcdata "Humidity : 77"];
     c_pressure = div [pcdata "Pressure : 1012.0"];
     c_lastBuildDate = div [pcdata "Last build date : Sat, 01 Jul 2017 01:08 PM EDT"]}*)

  let () =
    Dom_html.window##.onload := 
      Dom_html.handler (fun _ -> 
          let key_list = ["2379574";"2459115";"2487956";"2490383"] in
          cards := 
            key_list |> 
            List.map 
              (fun key -> 
                 let c = create_card () in
                 Html.Manip.appendChild 
                   (Html.Of_dom.of_element Dom_html.document##.body)
                   (wrap_card c);
                 ignore @@ getForecast key c ;
                 (key,c));
          Js._false);

    let self = ServiceWorker.create_self () in
    let p = self##.navigator##.serviceWorker##register (Js.string "service_worker.js") in
    (*let p = Service_register.register "service_worker.js" in*)
    ignore (
      Promise._then p
        ~catch:(fun _ -> 
            Promise.resolve_value (
              Firebug.console##log (Js.string "cannot be registered")))
        (fun _ -> 
           Promise.resolve_value (
             Firebug.console##log (Js.string "registered"))))
]

(*let%html manifest = "<link rel='manifest' href='/manifest.json'>"*)


let other_head =
  [ 
    (*link ~rel:[`Canonical] ~href:(Xml.uri_of_string "https://weather-pwa-sample.firebaseapp.com/final/") ();*)
    meta 
      ~a:[a_name "viewport";
          a_content "width=device-width, initial-scale=1, user-scalable=no"] 
      ();
    meta ~a:[a_name "apple-mobile-web-app-capable"; a_content "yes"] () ;
    meta ~a:[a_name "apple-mobile-web-app-status-bar-style"; a_content "black"] () ;
    meta ~a:[a_name "apple-mobile-web-app-title"; a_content "Weather"] () ;
    meta ~a:[a_name "msapplication-TileImage"; 
             a_content "images/icons/icon-144x144.png"] ();
    meta ~a:[a_name "msapplication-TileColor"; a_content "#2F3BA2"] ();
    link ~rel:[`Other "shortcut icon"] ~href:(make_uri ~service:(Eliom_service.static_dir ()) ["favicon.ico"]) ();
    link ~rel:[`Other "manifest"] ~href:(make_uri ~service:(Eliom_service.static_dir ()) ["manifest.json"]) ();
    link ~rel:[`Other "apple-touch-icon"] ~href:(Xml.uri_of_string "images/icons/icon-152x152.png") ()
  ]

let () =
  Pwaeliom_app.register
    ~service:main_service
    (fun () () ->
       let refresh_button = 
         button ~a:[a_class ["pwa-eliom-refresh"]] [pcdata "refresh"] in
       ignore ( 
         [%client 
           (let click_handler _ _ =
              List.iter 
                (fun x -> Lwt.async ( fun () -> getForecast (fst x) (snd x))) 
                !cards ;
              Lwt.return_unit
            in
            Lwt_js_events.clicks (Html.To_dom.of_element ~%refresh_button) click_handler 
            : unit Lwt.t)
         ]);
       Lwt.return
         (Eliom_tools.F.html
            ~title:"pwaeliom"
            (*~a:[a_manifest (make_uri ~service:(Eliom_service.static_dir ()) ["manifest.json"])]*)
            ~css:[["css";"pwaeliom.css"]]
            ~other_head:other_head
            Html.F.(body [
                div ~a:[a_class ["pwa-eliom-head"]] 
                  [h2 ~a:[a_class ["pwa-eliom-h2"]] [pcdata "PWA Ocsigen Demo"];
                   refresh_button];
                div ~a:[a_class ["pwa-eliom-container"]][
                  img ~a:[a_class ["pwa-eliom-city-img"]] 
                    ~alt:("Nanjing City")
                    ~src:(make_uri 
                            ~service:(Eliom_service.static_dir ())
                            ["images";"nanjing1.jpg"])
                    ()
                ]
              ])))
