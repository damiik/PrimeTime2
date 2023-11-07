type classes_list_t = {
  name: string;
  classes_list: string;
}


let classes_lists : classes_list_t list ref = ref []

let expand_classes (classes: string list) =
  let result =  
    List.fold_left(fun acc c -> 
      
      (match List.filter (fun (cl : classes_list_t) -> cl.name = c) !classes_lists with
        | [l] -> acc @ ((String.split_on_char ' ' l.classes_list))
        | _ -> acc @ c::[]
      )
    ) [] classes
  in
  result

