open Cool_lib (* modules: Styles, Icons *)

module T = Html_tools

(* dune runtest ./test --watch *)


let test_classes_lists() =

  let mariadb = Db.connect () in
  Db.get_classes_lists mariadb T.classes_lists;
  Db.close mariadb;

  let classes = ["class_a"; "grid-cell-lt"; "grid-cell-dk"; "class_c"] in
  let result = T.expand_classes  classes in
  assert ((result |> List.length) == 12 );
  assert (match (List.nth_opt result 10 ) with | Some s -> s = "bg-[#444]" | None -> false);
  (* Printf.printf "%s" (result |> List.fold_left (fun acc a -> Printf.sprintf "%s, <%s>"  acc a) ""); *)
  (* Printf.printf "%s" ((!T.classes_lists) |> List.fold_left (fun acc (a:T.classes_list_t) -> Printf.sprintf "%s, <%s>" acc a.name) ""); *)
  ()

(* Run all unit tests *)
let () =

  test_classes_lists();