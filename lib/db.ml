module M = Mariadb.Blocking
module T = Html_tools

exception DBError of M.error

type kind_of_record =  Article | Paragraph | Text | RawHtml

let kind_int k = (match k with
  | Article -> 0
  | Paragraph -> 1
  | Text -> 2
  | RawHtml -> 3
)

let kind_options = ["Article"; "Text"; "RawHtml"]
let kind_s k = match k with 
  | Article -> "Article"
  | Paragraph -> "Paragraph"
  | Text -> "Text" 
  | RawHtml -> "RawHtml"


type record_t = {

  id: int;
  title: string;
  author: string;
  kind_of: kind_of_record;
  mutable data: string;
  childs: int list;

}


let or_die = function
  | Ok x -> x
  | Error (num, msg) -> failwith (Printf.sprintf "DB error (\"or_die\" function) #%d: %s" num msg)


let print_row row =
  (* Printf.printf "---\n%!"; *)
  (M.Row.StringMap.iter
    (fun name field ->
      Printf.printf "%20s " name;
      match M.Field.value field with
      | `Int i -> Printf.printf "%d (int)\n%!" i
      | `Float x -> Printf.printf "%f (float)\n%!" x
      | `String s -> Printf.printf "%s (string)\n%!" s
      | `Bytes b -> Printf.printf "%s (bytes)\n%!" (Bytes.to_string b)
      | `Time t ->
          Printf.printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
            (M.Time.year t)
            (M.Time.month t)
            (M.Time.day t)
            (M.Time.hour t)
            (M.Time.minute t)
            (M.Time.second t)
      | `Null -> Printf.printf "NULL\n%!")
    row);
  ()

let get_dbhost () =  
  try Sys.getenv "SEOHOST_DBHOST"
  with
  | Not_found -> ""

let get_dbuser () =  
  try Sys.getenv "SEOHOST_DBUSER"
  with
  | Not_found -> ""
  
let get_dbpass () =  
  try Sys.getenv "SEOHOST_DBPASS"
  with
  | Not_found -> ""

let connect () = (* () unit is needed here if you like to get value instead of function as result *)
     
  M.connect
    ~host: (get_dbhost ())
    ~user: (get_dbuser ())
    ~pass: (get_dbpass ())
    ()
  |> or_die

let close db = M.close db

let update_article db (article : record_t) =

  (* let updateq = "UPDATE srv59515_baza01.Articles SET title = ? , data = ? , kind_of = ? , childs = ? WHERE id = ?" in *)
  Printf.printf "Update article2, id: %d, kind:%d, title:%s\n%!" article.id (article.kind_of |> kind_int) article.title;

  let stmt = M.prepare db "UPDATE srv59515_baza01.Articles SET title = ?, author = ?, data = ?,  kind_of = ?, childs = ? WHERE id = ?" |> or_die in
  [|
    `String article.title; 
    `String article.author;
    `String article.data;
    `Int (article.kind_of |> kind_int);
    `Int 0;
    `Int article.id;
  |] |> M.Stmt.execute stmt |> or_die |> ignore ;
  M.Stmt.close stmt |> or_die;

  ()

let get_data db (data : record_t list ref) = 

  (* let query = "SELECT * FROM srv59515_baza01.Articles WHERE title LIKE ? LIMIT ?" in *)
  (* let insertq = "INSERT INTO srv59515_baza01.Articles (title, author, kind_of, childs, data, time) VALUES (?, ?, ?, ?, ?, ?)" in *)
  (* let updateq = "UPDATE srv59515_baza01.Maszyny SET Komentarz1 = ? WHERE Nazwa = ?" in *)
  (* let deleteq = "Delete from srv59515_baza01.Maszyny WHERE Nazwa = ?" in *)
  (* let now = (Unix.time() |> M.Time.local_timestamp) in *)
  (* let values = [|`String "no title 5"; `String "anonymous"; `Int 0; `Null; `String html_string; `Time now |] in *)

  (* Execute the SQL query with the specified values *)
  (* ignore (M.Stmt.execute (M.prepare mariadb insertq |> or_die) values ) ; *)
  (* ignore (M.Stmt.execute (M.prepare mariadb updateq |> or_die) [|`String "New machine"; `String "M97";|] ) ; *)
  (* ignore (M.Stmt.execute (M.prepare mariadb deleteq |> or_die) [|`String "M97";|] ) ; *)

  let stmt = M.prepare db "SELECT * FROM srv59515_baza01.Articles WHERE title LIKE ? LIMIT ?" |> or_die in
  let res = M.Stmt.execute stmt [| `String "no title 2"; `Int 10 |] |> or_die in
  Printf.printf "number of rows: %d\n%!" (M.Res.num_rows res);

  let curr_data_id = ref 6 in
  let rec next () =
    (* Printf.printf "reading row..\n"; *)
    match M.Res.fetch (module M.Row.Map) res with
    | Ok (Some x) -> 
      print_row x;

      data :=  {
        kind_of = RawHtml; 
        childs = []; 
        title = x |> M.Row.StringMap.find "title" |> M.Field.bytes |> Bytes.to_string; 
        author = x |> M.Row.StringMap.find "author" |> M.Field.bytes |> Bytes.to_string; 
        data = x |> M.Row.StringMap.find "data" |> M.Field.bytes |> Bytes.to_string; 
        id = x |> M.Row.StringMap.find "id" |> M.Field.int 
      } :: (!data);
      curr_data_id := !curr_data_id + 1;     
      next ();
    | Ok None -> ()
    | Error e -> raise (DBError e); 
  in
  next ();
  (* Printf.printf "Close query..\n"; *)
  M.Stmt.close stmt |> or_die
 
let get_classes_lists db (classes_lists : T.classes_list_t list ref) =

  let stmt = M.prepare db "SELECT * FROM srv59515_baza01.classes_lists" |> or_die in
  let res = M.Stmt.execute stmt [| |] |> or_die in
  Printf.printf "number of rows: %d\n%!" (M.Res.num_rows res);

  let rec next () =
    (* Printf.printf "reading row..\n"; *)
    match M.Res.fetch (module M.Row.Map) res with
    | Ok (Some x) -> 
      print_row x;

      classes_lists :=  {
        name = x |> M.Row.StringMap.find "name" |> M.Field.string; 
        classes_list = x |> M.Row.StringMap.find "classes_list" |> M.Field.string 
      } :: (!classes_lists);
      next ();
    | Ok None -> ()
    | Error e -> raise (DBError e); 
  in
  next ();
  (* Printf.printf "Close query..\n"; *)
  M.Stmt.close stmt |> or_die;


  


(* Call this only once, before you're done using all
   your database handles. *)
M.library_end ();