
(*open Base*)
(* open Dream *)
(* open Lwt.Syntax *)
(* open Lwt.Infix *)
(* open Tyxml.Html *)
(* open Str *)


(* ocamlfind ocamlopt -thread -c -package base,str,dream,tyxml main.ml -linkpkg -o ./main.bytes *)
(* dune exec PrimeTime2 *)

let button_class = ["bg-green-800"; "hover:shadow none";"border-green-900";"border";"p-2";"m-3";"rounded";"shadow-lg";"shadow-[#020203]"] 

let body_class =["bg-stone-700"; "text-yellow-400"; "text-xl"; "font-['Nunito_Sans']" ] 

let grid_class =["bg-stone-900"; "grid";"grid-cols-4";"gap-4";"p-6"]

let del_icon =
  let open Tyxml.Svg in
  Tyxml.Html.svg ~a:[a_fill `None; a_viewBox (-10.0, -20.0, 80.0, 120.0); a_width (20.0, Some `Pt); a_height (24.0, Some `Pt);
    a_stroke `CurrentColor; a_stroke_width (8.0, Some `Pt); a_stroke_linecap `Round; a_stroke_linejoin `Round
  ]  (* ~a:[a_svg_ns; a_width 100; a_height 100;a_class ["w-3";"h-3";"text-white";"mr-2"] a_aria "hidden";*)
  [ 
    path ~a:[a_d "M -0.66145875,-3.9687501 H 72.098958"][];
    path ~a:[a_d "m 23.332953,-7.9954656 -0.006,-5.2337014 4.454297,-3.96875 h 17.197899 l 3.968767,3.96875 v 5.2916684"] [];
    path ~a:[a_d "m 0,6.614579 h 72.760417 l -13.22917,85.98959 h -46.30208 z"] [];
    path ~a:[a_d "m 26.458333,39.6875 19.84375,19.843752"] [];
    path ~a:[a_d "M 46.302083,39.6875 26.458333,59.531252"] []
  ]

let update_icon = 
  let open Tyxml.Svg in
  Tyxml.Html.svg ~a:[a_fill `CurrentColor; a_viewBox (0.0, 0.0, 100.0, 100.0);a_width (24.0, Some `Pt); a_height (16.0, Some `Pt);
    a_stroke `CurrentColor; a_stroke_width (2.0, Some `Pt); a_stroke_linecap `Round; a_stroke_linejoin `Round
  ]  (* ~a:[a_svg_ns; a_width 100; a_height 100;a_class ["w-3";"h-3";"text-white";"mr-2"] a_aria "hidden";*)
  [ 
    path ~a:[a_d "M77.926,94.924H8.217C6.441,94.924,5,93.484,5,91.706V21.997c0-1.777,1.441-3.217,3.217-3.217h34.854 c1.777,0,3.217,1.441,3.217,3.217s-1.441,3.217-3.217,3.217H11.435v63.275h63.274V56.851c0-1.777,1.441-3.217,3.217-3.217 c1.777,0,3.217,1.441,3.217,3.217v34.855C81.144,93.484,79.703,94.924,77.926,94.924z"][];
    path ~a:[a_d "M94.059,16.034L84.032,6.017c-1.255-1.255-3.292-1.255-4.547,0l-9.062,9.073L35.396,50.116 c-0.29,0.29-0.525,0.633-0.686,1.008l-7.496,17.513c-0.526,1.212-0.247,2.617,0.676,3.539c0.622,0.622,1.437,0.944,2.274,0.944 c0.429,0,0.858-0.086,1.276-0.257l17.513-7.496c0.375-0.161,0.719-0.397,1.008-0.686l35.026-35.026l9.073-9.062 C95.314,19.326,95.314,17.289,94.059,16.034z M36.286,63.79l2.928-6.821l3.893,3.893L36.286,63.79z M46.925,58.621l-5.469-5.469 L73.007,21.6l5.47,5.469L46.925,58.621z M81.511,24.034l-5.469-5.469l5.716-5.716l5.469,5.459L81.511,24.034z"][]
  ]

let head_el = 
  let open Tyxml.Html in
    (head 
      (title (txt "Greeting")) 
      [ script ~a:[a_src "https://unpkg.com/htmx.org@1.9.4"] (txt "");
        script ~a:[a_src "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio"] (txt "");
        script (txt "
        tailwind.config = {
          theme: {
            extend: {
              colors: {
                clifford: '#da373d',
                'stone': {
                  850: '#23201E'
                }
              }
            }
          }
        }");
        style [
          txt "
          @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono&display=swap');
          @import url('https://fonts.googleapis.com/css2?family=Nunito+Sans:opsz@6..12&display=swap');

          .scrolling-sidebar {
            height: calc(100vh - 2rem); /* Adjust the height as needed */
            overflow-y: auto;
            position: fixed;
          }";
        ]
      ]
    )


let sidebar_el = 
  let open Tyxml.Html in
  div ~a:[
    a_class["bg-stone-850";"text-white";"col-span-1"];
  ][
    div ~a:[a_class["p-4";"scrolling-sidebar"];] [
      (h1 ~a:[a_class["text-2xl";"font-semibold"]] [txt "Sidebar"]);
      ul ~a:[a_class ["mt-4"]] [
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Dashboard"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Products"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Customers"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Orders"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Settings22"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Dashboard"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Products"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Customers"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Orders"]];
        li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-lime-600"]][txt "Settings22"]]
      ]
    ]
  ]


let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt()) elt

let find_data key m =
  List.find_map (fun (header, data) ->
    if (header = key) then Some data else None) m

type kind_of_record_t =  Text | RawHtml


type record_t = {

  id: int;
  name: string;
  kind_of: kind_of_record_t;
  mutable data: string;
}
let kind_options = ["Text"; "RawHtml"]
let kind_s k = match k with | Text -> "Text" | RawHtml -> "RawHtml"


let edit_form request row = 
  let open Tyxml.Html in 
  let vals = Printf.sprintf "{\"row_id\":\"%d\",\"dream.csrf\":\"%s\"}" row.id (Dream.csrf_token request) in


  
  (*Dream.log "%s" vals;*)
  div [
    (textarea ~a:[
      a_name "edit_text"; a_id (Printf.sprintf "edit_text%d" row.id); a_class ["shadow_inner";"w-full";"bg-stone-800"];
      Unsafe.string_attrib "hx-post" "/line_update";
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-target" "closest .row_class"; (*closest parent element*) 
      Unsafe.string_attrib "hx-trigger" "keyup changed delay:20s";   
      Unsafe.string_attrib "hx-vals" vals;
      Unsafe.string_attrib "hx-include" (Printf.sprintf "#edit_text%d" row.id);
      Unsafe.uri_attrib "dream.csrf" (Dream.csrf_tag request); 
    ] (txt row.data));
    div ~a:[ a_class["w-full";"flex"; "flex-grow"; "flex-row"; "space-x-1"]] [
      button
        ~a:[
        Unsafe.string_attrib "hx-post" "/line_update";
        Unsafe.string_attrib "hx-swap" "outerHTML";
        (*Unsafe.string_attrib "hx-headers" "Content-Type: application/json";
        Unsafe.string_attrib "hx-params" (Printf.sprintf "{id:%d; data:%s}" row.id row.data);*)
        Unsafe.string_attrib "hx-target" "closest .row_class"; (*closest parent element*) (*row_class is a css class for main div with whole record row, as a target this div will be deleted, this could be #id as well *)
        Unsafe.string_attrib "hx-trigger" "click";   
        Unsafe.string_attrib "hx-vals" vals;
        Unsafe.string_attrib "hx-include" (Printf.sprintf "#edit_text%d" row.id);
        Unsafe.uri_attrib "dream.csrf" (Dream.csrf_tag request); 
        a_class button_class
        ] 
        [update_icon; txt "Update"];
      button
      ~a:[

        Unsafe.string_attrib "hx-delete" "/delete";
        Unsafe.string_attrib "hx-swap" "outerHTML";
        Unsafe.string_attrib "hx-target" "closest .row_class"; (*row_class is a css class for main div with whole record row, as a target this div will be deleted, this could be #id as well *)
        Unsafe.string_attrib "hx-trigger" "click";
        Unsafe.string_attrib "hx-vals" vals;
        a_class button_class 
      ]
      [del_icon; txt "Del";];
      select ~a:[a_class button_class

      ] (List.map (fun op -> option ~a:(if op = (kind_s row.kind_of) then [a_value op; a_selected ()] else [a_value op]) (txt op)) kind_options)
    ]
  ]


let display_id request row = 
  let open Tyxml.Html in 
  form
      ~a:[
      Unsafe.string_attrib "hx-post" "/edit"; 
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-trigger" "click";
      (*a_class["count"]*)] 
    [ Tyxml.Html.Unsafe.data (Dream.csrf_tag request);
      input ~a:[ a_hidden(); a_name "row_id"; a_value (Int.to_string row.id)] ();
      txt row.data]
;;

let display_row request row =

  let open Tyxml.Html in
  (*mark this div with css class "row_class", will be needed for remove whole record row, 
    while this is hx-target class /could be #id as weell/ *)
  div ~a:[a_class ["row_class"]] 
    [
    (*div ~a:[a_class["row_id"]] [txt (Int.to_string row.id)];*)
    div ~a:[a_class["name"]] [txt row.name];
    display_id request row;
    ]
;;

let index request list =
  let open Tyxml.Html in
  html head_el
    (body ~a:[a_class body_class] [
      
      div ~a:[a_class grid_class]  [
          
        sidebar_el;
        main ~a:[a_class ["col-span-3";"p-6";"rounded";"shadow"]] [
            
          div ~a:[a_class["list"]] (List.map (display_row request) list)
        ]
      ]
    ]
  )
;;



let process_row_by_form_id request data ~f =
  let open Lwt.Syntax in
  let* form = Dream.form (*~csrf:false*) request in
    (match form with 
      | `Ok [("row_id", id)] -> 

        let item_o = try Some( List.find (fun item -> item.id = int_of_string id ) !data ) with Not_found -> None in
        (match item_o with
          | None -> Dream.empty `Not_Found
          | Some item -> f item 
        )
      | _ -> Dream.empty `Bad_Request
    )


let () = 

  let data = ref [

    {name = "foo1"; kind_of = Text; data = "Zapytania HTTP mogą być generowane z dowolnych elementów (nie tylko z <a> lub <form>)"; id = 1};
    {name = "foo2"; kind_of = Text; data = "Zapytania HTTP mogą być genrewane przez dowolne zdarzenia (nie tylko przez \"click\" i \"submit\")"; id = 2};
    {name = "foo3"; kind_of = Text; data = "Dostępne są wszystkie metody AJAX (nie tylko POST i GET ale również PUT, PATCH, DELETE)"; id = 3};
    {name = "foo4"; kind_of = Text; data = "Zastępowana może być dowolna część dokumentu HTML (nie cały dokument)"; id = 4};
    {name = "foo5"; kind_of = Text; data = "Strony mogą być przeładowywane bez ponownego wczytywania nagłówków (a więc css'ów, fontów itp)."; id = 5};
    {name = "foo6"; kind_of = RawHtml; data = "Ogólnie idea jest taka, żeby odświeżać tylko elementy strony które wymagają odświeżenia"; id = 6};

  ] in

  let get_port () =  
    try int_of_string (Sys.getenv "PORT")
    with
    | Not_found -> 3000 in


  Dream.run ~interface:"0.0.0.0" ~port:( get_port() )
  (*Dream.run ~port:42069*)
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get "/" (fun request -> Dream.html @@ elt_to_string @@ index request !data);

    Dream.delete "/delete" (fun request ->
      process_row_by_form_id request data ~f:(fun item ->
        data := List.filter (fun x -> 
          x.id <> item.id) !data;
          Dream.empty `OK
        )
    );

    Dream.post "/edit" (fun request -> 
      process_row_by_form_id request data ~f:(fun item ->
         
        (*let v = int_of_string item.data in
        item.data <-  Printf.sprintf "%i" (v + 1);*)    
        Dream.html @@ elt_to_string @@ (edit_form request item)
      )
    );

    Dream.post "/line_update" (fun request -> 
      let open Lwt.Syntax in
      let* form = Dream.form request in
      match form with
        | `Ok form_data -> (*  [("row_id", "1"); ("edit_text", "....")]  *) 
          (match (find_data "row_id" form_data, find_data "edit_text" form_data) with 
            | (Some id, Some txt) ->
                 let item_o = try Some( List.find (fun item -> item.id = int_of_string id ) !data ) with Not_found -> None in
                 (match item_o with
                    | None -> Dream.empty `Not_Found
                    | Some item -> 
                      item.data <- txt;
                      Dream.html @@ elt_to_string @@ display_row request item)
            | _ -> 
              Dream.log "??Dream: Request data not match with <row_id> and <edit_text> parameters!\n";
              Dream.empty `OK)
        | _ ->
          Dream.log "??Dream: Request data not match with <`OK form_data>!\n";
      Dream.empty `OK 
    )

    ; Dream.get "/echo/:text"(fun request ->
        let text = Dream.param request "text" in
      
        (Dream.log "Dream run ---%s\n" text);
        Dream.empty `OK
      )
    
    ; Dream.post "/echo"(fun _ ->
      
        Printf.printf("Dream run ---\n");
        let open Tyxml.Html in
        let ocaml = a ~a:[a_href "ocaml.org"] [txt "OCaml!"] in
        Dream.html @@ elt_to_string ocaml
      )
    
    ; Dream.get "/end"(fun _ -> exit 0;)
    ]
;;
(*<button hx-trigger="click, keyup[altKey&&shiftKey&&key=='D'] from:body"
        hx-post="/doit">Do It! (alt-shift-D)</button>*)

(*
let response2 _ =
  let open Tyxml.Html in
  let ocaml = a ~a:[a_href "ocaml.org"] [txt "OCaml!"] in
  Dream.html @@ elt_to_string ocaml
;;
*)



(*let display_id request row = 
  let open Tyxml.Html in 
  let json_id = "{\"id\": " ^ Int.to_string row.id ^ "}" in
    div ~a:[
      Unsafe.string_attrib "hx-post" "/count"; 
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-vals" json_id;
      a_class["count"]] 
    [ Tyxml.Html.Unsafe.data (Dream.csrf_tag request);
      input ~a:[ a_hidden(); a_name "id"; a_value (Int.to_string row.id)] ();
      txt (Int.to_string row.count)]
*)  



(*

type todo = {
  id: int;
  title: string;
  mutable completed: bool;
}


let global_todos = ref []



let render_todo todo =
  let open Tyxml.Html in 
  let id = Int.to_string todo.id in 
  div
    [ div ~a:[ a_id id; a_class [ "todo"]] [ txt todo.title]
    ; div
        ~a:[Unsafe.string_attrib "hx-target" "foo"; a_class [ "completed"]]
      [txt @@ Bool.to_string todo.completed]
    ]
;;



let get_todos () =

  let open Tyxml.Html in 
  let todos =
    List.map render_todo !global_todos |> List.map(fun x -> li [ x ]) |> ul
  in 
  Dream.html @@ elt_to_string todos


let create_todo title = 
  let todo = { title; completed = false; id = List.length !global_todos } in 
  global_todos := 
        todo :: !global_todos;
  todo
;;


let complete_todo id = 
  let open Base in 
  match List.find !global_todos ~f:(fun x -> x.id = id) with
  | Some todo ->
      todo.completed <- true;
      Some todo
  | None -> None
;;


let handle_complete_todo id =
  let todo = complete_todo id in 
  match todo with
  | Some todo -> Dream.html @@ elt_to_string @@ render_todo todo
  | None -> Dream.empty `Not_Found
;;


let delete_todo id =
  let open Base in 
  match List.find !global_todos ~f:(fun x -> x.id = id) with
  | Some _ ->
      global_todos := List.filter !global_todos ~f:(fun x -> x.id <> id);
      true
  | None -> false
;;



let handle_delete_todo id =
  let todo = delete_todo id in
  match todo with
  | true -> Dream.empty `OK
  | false -> Dream.empty `Not_Found
;;

*)

(*
let response request =
  let open Lwt.Syntax in
  let* body = Dream.body request in
  Dream.respond ~headers:["Content-Type", "application/octet-stream"] body
*)


(*
     Dream.get "/" (fun _ -> get_todos())
    (*[ Dream.get "/" (fun _ -> html @@ elt_to_string @@ get_todos())*)

    ; Dream.post "/todo" (fun request ->
        let open Lwt.Syntax in
        let* form = Dream.form ~csrf:false request in
        match form with
          | `Ok [ ("title", title)] ->
            let id = create_todo title in
            Dream.html @@ elt_to_string @@ render_todo id
          | _ -> Dream.empty `Bad_Request)

    ; Dream.put "/todo" (fun request ->
        let open Lwt.Syntax in
        let* form = Dream.form ~csrf:false request in
        match form with
        | `Ok [ ("id", id) ] ->
            let id = int_of_string_opt id in
            (match id with
            | Some id -> handle_complete_todo id
            | _ -> Dream.empty `Bad_Request)
        | _ -> Dream.empty `Bad_Request)

    ; Dream.delete "/todo/:id" (fun request -> 
        let id = Dream.param request "id" in
        let id = int_of_string_opt id in
        (match id with
          | Some id -> handle_delete_todo id
          | _ -> Dream.empty `Bad_Request))
*)