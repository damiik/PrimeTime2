
(*open Base*)
(* open Dream *)
(* open Lwt.Syntax *)
(* open Lwt.Infix *)
(* open Tyxml.Html *)
(* open Str *)


(* ocamlfind ocamlopt -thread -c -package base,str,dream,tyxml main.ml -linkpkg -o ./main.bytes *)
(* dune exec PrimeTime2 *)

let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt()) elt


type row = {

  id: int;
  name: string;
  mutable count: int;
}

let display_id request row = 
  let open Tyxml.Html in 
  form
      ~a:[
      Unsafe.string_attrib "hx-post" "/count"; 
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-trigger" "click";
      a_class["count"]] 
    [ Tyxml.Html.Unsafe.data (Dream.csrf_tag request);
      input ~a:[ a_hidden(); a_name "row_id"; a_value (Int.to_string row.id)] ();
      txt (Int.to_string row.count)]
  
let display_delete request row = 
  let open Tyxml.Html in 
  
  form
    ~a:[

      Unsafe.string_attrib "hx-delete" "/delete";
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-target" "closest .row_class"; (*row_class is a css class for main div with whole record row, as a target this div will be deleted, this could be #id as well *)
      Unsafe.string_attrib "hx-trigger" "click";
      a_class["delete"] ]
    [
      Tyxml.Html.Unsafe.data (Dream.csrf_tag request);
      input ~a:[a_hidden (); a_name "row_id"; a_value (Int.to_string row.id)] (); (*hidden form element with name "row_id" takes value with row id*)
      button ~a:[a_style ""; a_class ["bg-green-800";"border-green-900";"border";"p-2";"m-3";"rounded";"shadow-lg";"shadow-[#020203]"]]
      [
       svg ~a:[Tyxml.Svg.a_fill `CurrentColor; Tyxml.Svg.a_viewBox (0.0, 0.0, 20.0, 16.0);Tyxml.Svg.a_width (24.0, Some `Pt); Tyxml.Svg.a_height (16.0, Some `Pt)]  (* ~a:[a_svg_ns; a_width 100; a_height 100;a_class ["w-3";"h-3";"text-white";"mr-2"] a_aria "hidden";*)
         [ 
          Tyxml.Svg.path ~a:[Tyxml.Svg.a_d "m10.036 8.278 9.258-7.79A1.979 1.979 0 0 0 18 0H2A1.987 1.987 0 0 0 .641.541l9.395 7.737Z"][];
          Tyxml.Svg.path ~a:[Tyxml.Svg.a_d "M11.241 9.817c-.36.275-.801.425-1.255.427-.428 0-.845-.138-1.187-.395L0 2.6V14a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2V2.5l-8.759 7.317Z"][]
         
          ];
        txt "Delete me Daddy "
      ];
      (*input ~a:[a_name "row_button"; a_value(Int.to_string row.id)] ()*) 
    ]

let display_row request row =

  let open Tyxml.Html in
  (*mark this div with css class "row_class", will be needed for remove whole record row, 
    while this is hx-target class /could be #id as weell/ *)
  div ~a:[a_class ["row_class"]] 
    [
    (*div ~a:[a_class["row_id"]] [txt (Int.to_string row.id)];*)
    div ~a:[a_class["name"]] [txt row.name];
    display_id request row;
    display_delete request row]
;;


let response2 _ =
  let open Tyxml in
  let ocaml = Html.(a ~a:[a_href "ocaml.org"] [txt "OCaml!"]) in
  Dream.html @@ elt_to_string ocaml
;;

let get_port () =  
  try int_of_string (Sys.getenv "PORT")
  
  with
  | Not_found -> 3000 
;;


let display_list request list =
  let open Tyxml.Html in
  div ~a:[a_class["list"]] (List.map (display_row request) list)
;;

let index request list =
  let open Tyxml.Html in
  html
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
              }
            }
          }
        }
      ");
      style [txt ".scrolling-sidebar {
                    height: calc(100vh - 2rem); /* Adjust the height as needed */
                    overflow-y: auto;
                    position: fixed;
                  }"]
      ])
    (body ~a:[a_style "background:#222222; color:#ffff00; font-size:22;"] [

      div ~a:[a_class ["grid";"grid-cols-4";"gap-4";"p-6"]]  [
          
        (*Sidebar*)
          div ~a:[a_class["bg-gray-900";"text-white";"col-span-1"];
            
            ][
            div ~a:[a_class["p-4";"scrolling-sidebar"];] 
              [(h1 ~a:[a_class["text-2xl";"font-semibold"]] [txt "Sidebar"]);
              ul ~a:[a_class ["mt-4"]] [
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Dashboard"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Products"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Customers"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Orders"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Settings22"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Dashboard"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Products"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Customers"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Orders"]];
                li ~a:[a_class ["mb-2"]] [a ~a:[a_href "#"; a_class ["hover:text-blue-500"]][txt "Settings22"]]
             ]
            ]
          ];

          main  ~a:[a_class ["col-span-3";"p-6";"rounded";"shadow"]] [
            
            display_list request list ;
            txt "dasdf";
            div [

            button ~a:[
              a_class ["bg-green-800";"border-green-900";"border";"p-2";"m-3";"rounded";"shadow-lg";"shadow-[#020203]"];
              Unsafe.string_attrib "hx-post" "/echo";
              Unsafe.string_attrib "hx-swap" "innerHTML";
              Unsafe.string_attrib "hx-target" "this"] [txt "Aktualizuj"];
            div [ txt "Stara zawartość"]
            ]
          ]
        ]
      ]); 

;;

let handle_id_request request data ~f =
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

    {name = "foo1"; count = 1; id = 1};
    {name = "foo2"; count = 1; id = 2};
    {name = "foo3"; count = 1; id = 3};
    {name = "foo4"; count = 1; id = 4};
    {name = "foo5"; count = 1; id = 5};
    {name = "foo6"; count = 1; id = 6};

  ] in
  Dream.run ~interface:"0.0.0.0" ~port:( get_port() )
  (*Dream.run ~port:42069*)
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get "/" (fun request -> Dream.html @@ elt_to_string @@ index request !data);

    Dream.delete "/delete" (fun request ->
      handle_id_request request data ~f:(fun item ->
        data := List.filter (fun x -> 
          x.id <> item.id) !data;
          Dream.empty `OK
        )
    );

    Dream.post "/count" (fun request -> 
      handle_id_request request data ~f:(fun item ->
          
        item.count <- item.count + 1;
        Dream.html @@ elt_to_string @@ (display_id request item)
      )
    )

    ; Dream.get "/echo/:text"(fun request ->
        let text = Dream.param request "text" in
      
        (Dream.log "Dream run ---%s\n" text);
        Dream.empty `OK
      )
    ; Dream.post "/echo"(fun _ ->
      
        Printf.printf("Dream run ---\n");
        response2()
      )
    ; Dream.get "/end"(fun _ -> exit 0;)
    ]
;;


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