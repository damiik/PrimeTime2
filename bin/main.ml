open Cool_lib (* modules: Styles, Icons *)
open Layouts  (* modules: Sidebard, Head *)
open Html_parser

(*todo*)
(*
html element attributes without values   
required newlines between tags
*)


let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt()) elt

let find_data key m =
  List.find_map (fun (header, data) ->
    if (header = key) then Some data else None) m

type kind_of_record_t =  Article | Paragraph | Text | RawHtml


type record_t = {

  id: int;
  name: string;
  kind_of: kind_of_record_t;
  mutable data: string;
  childs: int list;
}

let kind_options = ["Article"; "Text"; "RawHtml"]
let kind_s k = match k with 
  | Article -> "Article"
  | Paragraph -> "Paragraph"
  | Text -> "Text" 
  | RawHtml -> "RawHtml"


let edit_form request row = 
  let open Tyxml.Html in 
  let vals = Printf.sprintf "{\"row_id\":\"%d\",\"dream.csrf\":\"%s\"}" row.id (Dream.csrf_token request) in

  (*Dream.log "%s" vals;*)
  div [
    (textarea ~a:[
      a_name "edit_text"; a_id (Printf.sprintf "edit_text%d" row.id); a_class Styles.edit_text_class;
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
        a_class Styles.button_class
        ] 
        [Icons.update_icon; txt "Update"];
      button
      ~a:[

        Unsafe.string_attrib "hx-delete" "/delete";
        Unsafe.string_attrib "hx-swap" "outerHTML";
        Unsafe.string_attrib "hx-target" "closest .row_class"; (*row_class is a css class for main div with whole record row, as a target this div will be deleted, this could be #id as well *)
        Unsafe.string_attrib "hx-trigger" "click";
        Unsafe.string_attrib "hx-vals" vals;
        a_class Styles.button_class 
      ]
      [Icons.del_icon; txt "Del";];
      select ~a:[a_class Styles.button_class

      ] (List.map (fun op -> option ~a:(if op = (kind_s row.kind_of) then [a_value op; a_selected ()] else [a_value op]) (txt op)) kind_options)
    ]
  ]


(* helper function for searching key with attribute and appl. fun. f to value and return attrib list *)
let search_attr a f (l, r) = (match List.filter (fun (n,_) -> n = a) l with
  | [(_,s)] -> (l, (f s)::r)
  | _ -> (l, r))

let get_a_attrib attrib_list =  
  let open Tyxml.Html in 
  let (_, res) = (attrib_list, []) |> 
  (search_attr "class"  (fun a -> a_class (String.split_on_char ' ' a))) |> (* wrap function to convert argument to list of classes *)
  (search_attr "style"   a_style) in
  res

let get_href_attrib attrib_list =  
  let open Tyxml.Html in 
  let (_, res) = (attrib_list, []) |> 
  (search_attr "class"  (fun a -> a_class (String.split_on_char ' ' a))) |> (* wrap function to convert argument to list of classes *)
  (search_attr "href"     a_href) |>
  (search_attr "style"   a_style) in
  res


let get_iframe_attrib attrib_list =  
  let open Tyxml.Html in 

  let (_, res) = (attrib_list, []) |> 
  (search_attr "class"  (fun a -> a_class (String.split_on_char ' ' a))) |> (* wrap function to convert argument to list of classes *)
  (search_attr "style"   a_style) |>
  (search_attr "title"   a_title) |>
  (search_attr "src"     a_src) |>
  (search_attr "allow"  (fun _ -> a_name "")) |> (* ignored *)
  (search_attr "frameborder"  (fun _ -> a_name "" )) |>  (* ignored *)
  (search_attr "width"  (fun a -> a_width  (int_of_string a))) |>  (* wrap function to convert argument to int *)
  (search_attr "height" (fun a -> a_height (int_of_string a))) |>  (* wrap function to convert argument to int *)
  (search_attr "allowfullscreen" (fun _ -> a_allowfullscreen ())) in (* wrap function to ignore argument of a_allowfullscreen *)
  res


let rec xml_to_elt2 xml =   
  let open Tyxml.Html in
  let l : [< Html_types.core_phrasing_without_interactive ] elt option = 
  match xml with 
  | Parser.Tag_El el -> (
    match el.name with
    | "strong" -> Some (strong ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "small" -> Some (small ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "em" -> Some (em ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "u" -> Some (u ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "b" -> Some (b ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "i" -> Some (i ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "span" -> Some (span ~a:(get_a_attrib el.attributes) (List.fold_left get_childs2 [] el.childs))
    | "br" -> Some (br ~a:(get_a_attrib el.attributes) ())
    | "wbr" -> Some (wbr ~a:(get_a_attrib el.attributes) ())
    | _ -> None
    )
  | Text_El t -> Some (txt t)
  in
  l

and get_childs2 l ch = 
  match (xml_to_elt2 ch) with 
  | Some e -> e::l 
  | None -> l


let xml_to_elt_li xml =   
    let open Tyxml.Html in
    match xml with 
    | Parser.Tag_El el -> (
      match el.name with
      | "li" -> (li ~a:(get_a_attrib el.attributes) (List.fold_left (fun l ch -> match (xml_to_elt2  ch) with |Some e -> e::l| None -> l) [] el.childs))
      | n -> li [txt n]
    )
    | Text_El t -> li [txt (String.concat "-" [t ;"??li"])] 


  (* type t3 = [< Html_types.div_content_fun >`Div| `H1 |`H2 |`H3| `H4 |`H5 |`H6 |`P |`Ul ] elt list *)
let rec xml_to_elt xml =   
  let open Tyxml.Html in
  let l2 :([< Html_types.div_content ] elt) option = 
  match xml with 
  | Parser.Tag_El el -> (

    match el.name with
    | "h1" -> Some (h1 ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))
    | "h2" -> Some (h2 ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))
    | "h3" -> Some (h3 ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))
    | "h4" -> Some (h4 ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))
    | "h5" -> Some (h5 ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))
    | "h6" -> Some (h6 ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))  
    | "p" -> Some (p ~a:(get_a_attrib el.attributes) (List.fold_left get_phrasing_ch [] el.childs))
    | "ul" -> Some (ul ~a:(get_a_attrib el.attributes) (List.fold_left (fun l ch -> (xml_to_elt_li ch)::l) [] el.childs))
    | "a" -> Some (a ~a:(get_href_attrib el.attributes) (List.fold_left get_without_interactive_ch [] el.childs))
    | "iframe" -> Some (iframe ~a:(get_iframe_attrib el.attributes) [])
    | "div" -> 
      Some (div ~a:(get_a_attrib el.attributes) ( 
        (List.fold_left get_childs2 [] el.childs) @ 
        (List.fold_left get_flow_ch [] el.childs) 
      )) (* xml_to_elt must be added also here *)
      
    | "blockquote" -> 

      Some (blockquote ~a:(get_a_attrib el.attributes) ( 
        (List.fold_left get_childs2 [] el.childs) @ 
        (List.fold_left get_flow_ch [] el.childs) 
      )) (* xml_to_elt must be added also here *)
      | _ -> None
    )
  | Text_El t -> Some (div [txt (String.concat "-" [t ;"??div"])])
  in 
  l2

and get_childs2 (l (*:[< Html_types.phrasing > `B `Br `I `PCDATA `Span `U ] Tyxml_html.elt Tyxml_html.list_wrap*) )  ch = 
  match (xml_to_elt2 ch) with 
    | Some e -> (e :> Html_types.body_content_fun Tyxml_html.elt) :: l  (*promote this type to body_content*)
    | None -> l

and get_phrasing_ch l ch = 
    match (xml_to_elt2 ch) with 
    | Some e -> e::l 
    | None -> l 
  
and get_without_interactive_ch l ch = 
    match (xml_to_elt2 ch) with 
    | Some e -> (e :> Html_types.flow5_without_interactive Tyxml_html.elt )::l
    | None -> l 
  
and get_flow_ch l ch = 
    match (xml_to_elt ch) with 
    | Some e -> e::l (*(e :> Html_types.flow5 Tyxml_html.elt) ::l *)
    | None -> l     
let html_string = 
 (* {|<body class="bg-stone-700 text-yellow-400 text-xl font-['Nunito_Sans']"><div  class="bg-stone-900 grid grid-cols-4 gap-4 p-6"><div class="bg-stone-850 text-white col-span-1"><div class="p-4 scrolling-sidebar"><h1 class="text-2xl font-semibold">Sidebar</h1><ul class="mt-4"><li class="mb-2"><a href="#" class="hover:text-lime-600">Dashboard</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Products</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Customers</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Orders</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Settings22</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Dashboard</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Products</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Customers</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Orders</a></li><li class="mb-2"><a href="#" class="hover:text-lime-600">Settings22</a></li></ul></div></div><main class="col-span-3 p-6 rounded shadow"><div class="list"><div class="row_class"><div class="name">foo1</div><div hx-post="/edit" hx-swap="outerHTML" hx-trigger="click[ctrlKey]" hx-vals="{&quot;row_id&quot;:&quot;1&quot;,&quot;dream.csrf&quot;:&quot;ADWnGwb3lRsn0_jqFbxk_k88vE2RTovl9Q4AZKR29UfN-H45TsRgLfaRGS6maS16aktp9txD8Srk_Un_1ZYAWcy4lIvpbaSClOGo571HpiVO&quot;}">Zapytania HTTP mogą być generowane z dowolnych elementów (nie tylko z &lt;a&gt; lub &lt;form&gt;)</div></div><div class="row_class"><div class="name">foo2</div><div hx-post="/edit" hx-swap="outerHTML" hx-trigger="click[ctrlKey]" hx-vals="{&quot;row_id&quot;:&quot;2&quot;,&quot;dream.csrf&quot;:&quot;AG-limB7nv-J4mFC5PqqKY4zj3jZIa8gJJBeVc4ecjtXjq73bw1vImD8JLeKapV7Po-Eh5HBhZAZpaB9frR628Jj-9vtlHqdV-PoCdCMVnas&quot;}">Zapytania HTTP mogą być genrewane przez dowolne zdarzenia (nie tylko przez &quot;click&quot; i &quot;submit&quot;)</div></div><div class="row_class"><div class="name">foo4</div><div hx-post="/edit" hx-swap="outerHTML" hx-trigger="click[ctrlKey]" hx-vals="{&quot;row_id&quot;:&quot;4&quot;,&quot;dream.csrf&quot;:&quot;ANThgIAIBdGxw0TlGes8pMzaHeQtOMRoKVp6wy3HxWGMOMtkAk_IPfjgSl0wjzViO-wPhTRRDxv7w4EHjV1Y2bzNts54xmvXsIYw093ngyz0&quot;}">Zastępowana może być dowolna część dokumentu HTML (nie cały dokument)</div></div><div class="row_class"><div class="name">foo5</div><div hx-post="/edit" hx-swap="outerHTML" hx-trigger="click[ctrlKey]" hx-vals="{&quot;row_id&quot;:&quot;5&quot;,&quot;dream.csrf&quot;:&quot;AKTD3YnxAfokc2fFa3be-QoHJfcNIAiOK506Brx99xCEmL6rDi0pxR4Q6gUaU4I9RorQpZbBsNgzCCrMWXv4CztZSJPZuZRDgCdFIGZ8hWQ6&quot;}">Strony mogą być przeładowywane bez ponownego wczytywania nagłówków (a więc css'ów, fontów itp).</div></div><div class="row_class"><div class="name">foo6</div><div hx-post="/edit" hx-swap="outerHTML" hx-trigger="click[ctrlKey]" hx-vals="{&quot;row_id&quot;:&quot;6&quot;,&quot;dream.csrf&quot;:&quot;AD7JVtNspWcfTkr6b-BGwX-chgGf33HbHUw13x0ue1tY1NEqxXbZdM35D7WfTGb6FqR-eEQHnY_kvFcPL1MVKHW-Y8JJggEp8DYXUNG5KYxg&quot;}">&lt;p&gt;Ogólnie &lt;b&gt;idea&lt;/b&gt; jest taka, żeby &lt;i&gt;odświeżać&lt;/i&gt; tylko elementy strony które wymagają odświeżenia&lt;/p&gt;</div></div></div></main></div></body>
 
 |} *)
 
(* {|<div class="bg-lime-900">
    <p>Ogólnie <b>idea</b> jest taka, żeby <i>odświeżać</i> tylko elementy strony które wymagają odświeżenia</p>
    <div class="bg-back">
       <p> Oto przykład listy: </p>
    </div>
    <ul>
        <li><b>1. A</b></li>
        <li><b>2. B</b></li>
        <li><b>3. C</b></li>
    </ul>
</div>
|}  *){|<blockquote class="bg-[#232123] shadow-2xl shadow-2xl p-3 rounded-md">
<div style="color: #ede0ce;font-family: 'JetBrainsMono Nerd Font Mono', 'Droid Sans Mono', 'monospace', monospace;font-weight: normal;font-size: 20px;line-height: 30px; white-space: pre;">
  <div><span style="color: #7a7267;">(* Copyright by Dariusz Mikołajczyk 2024 *)</span></div>
  <div><span style="color: #92b55f;">type</span><span style="color: #ede0ce;"> </span>
<span style="color: #e8da5e;">token</span><span style="color: #ede0ce;"> </span>
<span style="color: #92b55f;">=</span><span style="color: #ede0ce;"> </span></div>
  <div><span style="color: #ede0ce;"> </span><span style="color: #a0988e;">|</span><span style="color: #ede0ce;">
    </span><span style="color: #487d76;">Tok_Less</span><span style="color: #ede0ce;"> </span><span  style="color: #a0988e;">of</span>
<span style="color: #ede0ce;"> </span><span style="color: #487d76;">int</span><span style="color: #ede0ce;"> </span>
<span style="color: #92b55f;">*</span><span style="color: #ede0ce;"> </span><span style="color: #487d76;">int</span>
  </div>
</div>
<p class="flex justify-center">
<iframe width="1854" height="756" src="https://www.youtube.com/embed/YMuBBEMV-7M?list=RDYMuBBEMV-7M" title="Lady, Lady, Lady - Joe Esposito (Ana de Armas)" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen="" >
</iframe>
</p>
</blockquote>
|} 
(* {|<div style="color: #ede0ce;background-color: #1a1b1d;font-family: 'JetBrainsMono Nerd Font Mono', 'Droid Sans Mono', 'monospace', monospace;font-weight: normal;font-size: 20px;line-height: 30px; white-space: pre;">
<div>
  <span style="color: #7a7267;">(* Copyright by Dariusz Mikołajczyk 2024 *)</span>
</div>
<div> 
  <div>
  <span style="color: #92b55f;">type</span>
  <span style="color: #ede1ce;"> </span>
  <span style="color: #e8da5e;">token</span>
  <span style="color: #ede0ce;"> </span>
 </div><div>
  <span style="color: #92b55f;">=</span>
  <span style="color: #ede0ce;"> </span>
 <br/><br/><br/>
  <span>Ala ma kota </span>
 </div>
</div>
</div>
|} *)
let display_row request row =
  let vals = Printf.sprintf "{\"row_id\":\"%d\",\"dream.csrf\":\"%s\"}" row.id (Dream.csrf_token request) in
 
(*  let tokens : Lexer.token list = Lexer.tokenize {|<h1 style="text-align: center;" name="dupa"><a>abc</a></h1>|} *)
(*  let tokens : Lexer.token list = Lexer.tokenize {|<h5 style="text-align: left;"><em>👉 </em><span style="color: #236fa1;"><em><a style="color: #236fa1;" title="Obliczenia strat dynamicznych dla mosfet " href="https://www.elektroda.pl/rtvforum/topic3474295.html">Obliczenia strat dynamicznych dla mosfet</a></em></span></h5>|}*)

let tokens : Lexer.token list = Lexer.tokenize html_string
in

  (* Dream.log "%s" (Lexer.tokensl2str tokens); *)
  let _ = match (Array.of_list tokens |> ref) |> Parser.parser_run Parser.tag_element_p with
    | Ok el  ->
      Dream.log "\n\n------------------------------\n%s\n------------------------------\n" (Parser.pp el "  " "  ")
    | Error {desc =e; token_ix=_} -> 
      Dream.log "\n\n------------------------------\nHtml parser error:%s\n------------------------------\n" e 
    in


  let open Tyxml.Html in
  let row_div = if row.kind_of = RawHtml then 
    let tokens : Lexer.token list = Lexer.tokenize row.data in
    (* Dream.log "%s" (Lexer.tokensl2str tokens); *)
    match( Array.of_list tokens |> ref) |> Parser.parser_run Parser.tag_element_p with
    | Ok e -> (match (xml_to_elt e) with

          |Some e' -> e'
          |None -> (div [txt ""])
    )
    | Error {desc =e; token_ix=_} -> (txt  (Printf.sprintf "Html parser error:%s" e ))
  
  else (txt row.data) in
  (*mark this div with css class "row_class", will be needed for remove whole record row, 
    while this is hx-target class /could be #id as weell/ *)
  div ~a:[a_class ["row_class"]] [

    div ~a:[a_class["name"]] [txt row.name];
    div ~a:[
      Unsafe.string_attrib "hx-post" "/edit"; 
      Unsafe.string_attrib "hx-swap" "outerHTML";
      Unsafe.string_attrib "hx-trigger" "click[ctrlKey]";
      Unsafe.string_attrib "hx-vals" vals;]
    [ 
      (*Tyxml.Html.Unsafe.data (Dream.csrf_tag request);*)
      row_div
    ]
  ]
;;


let index request article_id data_ref =
  let open Tyxml.Html in
  let article_o = try Some( List.find (fun item -> item.id = article_id) !data_ref ) with Not_found -> None in
  (match article_o with
    | None -> p [txt "Article not found"]
    | Some article -> 

    let elements_l = (List.map (fun id -> 
      let el_o = try Some( List.find (fun item -> item.id = id) !data_ref ) with Not_found -> None in
      (match el_o with
        | None -> {name = "root"; kind_of = Paragraph; childs = []; data = "*** Element child not found ***"; id = 7};
        | Some el ->  el
      
      )
    ) article.childs) in

    html Head.el
      (body ~a:[a_class Styles.body_class] [
      
        div ~a:[a_class Styles.grid_class]  [
          
          Sidebar.el;
          main ~a:[a_class ["col-span-3";"p-6";"rounded";"shadow"]] [
            
            div ~a:[a_class["list"]] (List.map (display_row request) elements_l)
          ]
        ]
      ]
    )
  )
;;



let process_record_t request data ~f =
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
    {name = "root"; kind_of = Article; childs = [1;2;4;5;6]; data = "This is article about Htmx"; id = 0};
    {name = "root"; kind_of = Paragraph; childs = []; data = "***"; id = 7};

    {name = "foo1"; kind_of = Text; childs = []; data = "Zapytania HTTP mogą być generowane z dowolnych elementów (nie tylko z <a> lub <form>)"; id = 1};
    {name = "foo2"; kind_of = Text; childs = []; data = "Zapytania HTTP mogą być genrewane przez dowolne zdarzenia (nie tylko przez \"click\" i \"submit\")"; id = 2};
    {name = "foo3"; kind_of = Text; childs = []; data = "Dostępne są wszystkie metody AJAX (nie tylko POST i GET ale również PUT, PATCH, DELETE)"; id = 3};
    {name = "foo4"; kind_of = Text; childs = []; data = "Zastępowana może być dowolna część dokumentu HTML (nie cały dokument)"; id = 4};
    {name = "foo5"; kind_of = Text; childs = []; data = "Strony mogą być przeładowywane bez ponownego wczytywania nagłówków (a więc css'ów, fontów itp)."; id = 5};
    {name = "foo6"; kind_of = RawHtml; childs = []; data = html_string;
    id = 6};

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

    Dream.get "/" (fun request -> Dream.html @@ elt_to_string @@ (index request 0 data));

    Dream.delete "/delete" (fun request ->
      process_record_t request data ~f:(fun item ->
        data := List.filter (fun x -> 
          x.id <> item.id) !data;
          Dream.empty `OK
        )
    );

    Dream.post "/edit" (fun request -> 
      process_record_t request data ~f:(fun item ->
         
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

(*
open Tyxml.Xml
open Tyxml_html
open Xml
  
let parse_html_string (html_string: string)  =
    let parser = new XmlParser () in
    let document = parser#parse_string html_string in
    let root_element = document#root in
    cast_element root_element
*)
(* hx-trigger="document.querySelector('#my-button').textContent === 'Click me'*)

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