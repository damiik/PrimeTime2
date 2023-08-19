
(*open Base*)
(* open Dream *)
(* open Lwt.Syntax *)
(* open Lwt.Infix *)
(* open Tyxml.Html *)
(* open Str *)


(*ocamlfind ocamlopt -thread -c -package base,str,dream,tyxml main.ml -linkpkg -o ./main.bytes*)


let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt()) elt

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



type row = {

  id: int;
  name: string;
  mutable count: int;
}
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

let display_id request row = 
  let open Tyxml.Html in 
  form
      ~a:[
      Unsafe.string_attrib "hx-post" "/count"; 
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-trigger" "click";
      a_class["count"]] 
    [ Tyxml.Html.Unsafe.data (Dream.csrf_tag request);
      input ~a:[ a_hidden(); a_name "id"; a_value (Int.to_string row.id)] ();
      txt (Int.to_string row.count)]
  



let display_row request row =

  let open Tyxml.Html in

  div [
    div ~a:[a_class["id"]] [txt (Int.to_string row.id)];
    div ~a:[a_class["name"]] [txt row.name];
    display_id request row]
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
      [ script ~a:[a_src "https://unpkg.com/htmx.org@1.9.4"] (txt "")])
    (body [display_list request list])
;;


let () = 

(*  let _ = create_todo "First ToDo!" in
  let _ = create_todo "And Another One!" in *)

  let data = ref [

    {name = "foo1"; count = 1; id = 1};
    {name = "foo2"; count = 1; id = 2};
    {name = "foo3"; count = 1; id = 3};
    {name = "foo4"; count = 1; id = 4};
    {name = "foo5"; count = 1; id = 5};
    {name = "foo6"; count = 1; id = 6};
    {name = "foo7"; count = 1; id = 7};
    {name = "foo8"; count = 1; id = 8};
    {name = "foo9"; count = 1; id = 9};
    {name = "foo10"; count = 1; id = 10}

  ] in

  Dream.run ~interface:"0.0.0.0" ~port:( get_port() )
  (*Dream.run ~port:42069*)
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get "/" (fun request -> Dream.html @@ elt_to_string @@ index request !data);
    Dream.post "/count" (fun request -> 

      let open Lwt.Syntax in
      let* form = Dream.form (*~csrf:false*) request in
      (match form with 
        | `Ok [("id", id)] -> 
                
          let item_o = try Some( List.find (fun item -> item.id = int_of_string id ) !data ) with Not_found -> None in
          (match item_o with
            | None -> Dream.empty `Not_Found
            | Some item -> 
              item.count <- item.count + 1;
              Dream.html @@ elt_to_string @@ (display_id request item)
          )
        | _ -> Dream.empty `Bad_Request
      )
    )

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
    ; Dream.post "/echo"(fun _ -> response2())
    ]
;;
