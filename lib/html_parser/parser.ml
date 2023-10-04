(* Copyright by Dariusz Mikołajczyk 2023 *)

(*-------------------------------------------------------------------------
Html parser 
--------------------------------------------------------------------------*)

(* 
It is not full Html parser. 
It's rather simple basic xml parser. 
Just what I need for this application. 
*)

open Printf
open Lexer

exception ParserError of string

type state = { 
  
  tokens: token array ref;
  token_ix: int;
  byte_counter: int;
}


let set_state (t, i, c) = {tokens = t; token_ix = i; byte_counter = c; }

(* increment token_ix *)
let state_next_token s = {s with token_ix = s.token_ix + 1}

(* increment token_ix and update byte counter *)
let state_next_token_w s w = {s with token_ix = s.token_ix + 1; byte_counter = s.byte_counter + w; }


type error = { 

  desc: string;
  token_ix: int
}

type 'a parser = {

  run : state -> state * ('a, string) result
}

let fail (e: string) = { 
  run = fun state -> 
    let token = !( state.tokens ).( state.token_ix ) in
    let (row, col) = token2pos token in
    state, Error (sprintf ": %s: %s at: (row:%d, col:%d) " e (token2str token) row col) 
}

let return (x: 'a) = { run = fun state -> state, Ok x}

let get_state p  = { run = fun state ->  

  printf "get_state token:%s byte:%d\n" (
    if state.token_ix < (Array.length !( state.tokens )) 
    then  (token2str !( state.tokens ).( state.token_ix )) 
    else ">>end<<" ) state.byte_counter;
  match (state |> p.run) with
  | (state', Ok res) -> state', Ok (res, state')
  | (_, Error e) -> state, Error e
}

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun state ->
          match p.run state with
          | state', Ok x -> state', Ok (f x)
          | _, Error error -> state, Error error (* if fail, return old state token_ix *)
  }


let ( >>= )  (p: 'a parser) (f: 'a -> 'b parser)  : 'b parser  = { 
  
  run = fun state -> 
      let state', result = p.run state in
      match result with
      | Ok x -> (f x).run state'
      | Error error -> state', Error error (* if fail, return old state token_ix *)
}


let bind p f = p >>= f


let ( <|> ) (p1: 'a parser) (p2: 'a parser): 'a parser = { 
  
  run = fun state -> 
    match state |> p1.run with 
    | _, Error e1 -> 
      (match state |> p2.run with
          | _, Error e2 -> (state, Error (sprintf "%s or \n\t%s" e1 e2))
          | res2 -> res2
      )
    | res1 -> res1
}

(* don't care of result from left side parser *)
let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser = { run = fun state -> 
  state |> (p1 >>= (fun _ -> p2 >>= (fun b -> return b))).run } (* this last run is run from bind operator >>= *)

(* don't care of result from right side parser *)
let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser = { run = fun state -> 
  state |> (p1 >>= (fun a -> p2 >>= (fun _ -> return a))).run }

(* pair of left and right side will be used *)
let ( <*> ) (p1: 'a parser) (p2: 'b parser): ('a * 'b) parser  = { run = fun state -> 
  state |> (p1 >>= (fun a -> p2 >>= (fun b -> return (a,b)))).run }

(* this parser is also used as an option parser - always list will be returned (maybe empty)*)
let zeroOrMore (p:'a parser) : 'a list parser = {
  run = fun state -> 

    let rec parse_fun = fun i -> 
      let state', result = p.run i in
      match result with
      | Error _ -> i, Ok []       
      | Ok x -> let state'', result' = parse_fun state' in
          (match result' with
          | (Error _) -> (state', Ok (x::[]))       
          | ( Ok x'') -> (state'', Ok ( x::x'')))
    in
    match parse_fun state with
    | (s, Error e) ->  (s, Error e)
    | (s, Ok final) ->  (s, Ok (List.rev final)) (*reverse result*)
}  

let oneOrMore (p:'a parser) : 'a list parser = {
  run = fun state -> 
    
    let rec parse_fun = fun (curr_state: state) -> 
      assert (curr_state.token_ix < (Array.length !( curr_state.tokens )));
      let state', result = p.run curr_state in
      match result with
      | Error s ->  curr_state, Error s       
      | Ok x -> let state'', result' = parse_fun state' in
          (match result' with
          | (Error s) -> printf "******** Error: %s\n" s;
            (state', Ok (x::[]))       
          | ( Ok x'') -> (state'', Ok ( x::x'')))
    in
    match parse_fun state with
    | (s, Error e) ->  (s, Error e)
    | (s, Ok final) ->  (s, Ok (List.rev final)) (*reverse result*)
}    

let is_a (t: token) : bool parser = {
  run = fun state -> 

    assert (state.token_ix < (Array.length !( state.tokens )));
    let curr_token = !( state.tokens ).( state.token_ix ) in
    let (line, column) = Lexer.token2pos curr_token in
    (* printf "is_a %s ? %s\n"  (token2str t) (token2str curr_token); *)
    if (tokenCompare !( state.tokens ).( state.token_ix ) t true)  then  (

      (state_next_token state), Ok true
    )
    else 
      state, Error ( sprintf ": Not a %s at: (line:%d, col:%d) [%s] " (t |> token2str) line column  (token2str curr_token))
}



(*-------------------------------------------------------------------------
Xml parser 
--------------------------------------------------------------------------*)

type xml_object =
  | Text_El of string
  | Tag_El of tag_element

and tag_element = {
  name : string;
  attributes : (string * string) list;
  mutable childs : xml_object list
}

(* tag name, attribute name. Have only letters, digits, '_' and '-' characters *)
let name_p: string parser = {
  run = fun state -> 
   
    let curr_token = !( state.tokens ).( state.token_ix ) in
    let (line, column) = Lexer.token2pos curr_token in
    match curr_token with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_Word (l_str, _, _) -> 
         {state with token_ix = (state.token_ix + 1); }, Ok l_str
    | token -> 
      state, Error ( sprintf ": Name (Tok_Word) expected but got: >%s< at: (line:%d, col:%d) " (token2str token) line column)
}

(* tag name, attribute name. Have only letters, digits, '_' and '-' characters *)
let keyword_p s: string parser = {
  run = fun state -> 
    let curr_token = !( state.tokens ).( state.token_ix ) in
    let (line, column) = Lexer.token2pos curr_token in
    match curr_token with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_Word (l_str, _, _) -> 
      if l_str = s then {state with token_ix = (state.token_ix + 1); }, Ok l_str
      else state, Error ( sprintf ": Keyword >%s< expected but got keyword: >%s< at: (lin:%d, col%d)" s l_str line column)
    | token -> 
      state, Error ( sprintf ": Keyword >%s< expected but got token: >%s< at: (line:%d, col:%d)" s (token2str token) line column)
}

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq
  
let char_list_to_string (l:char list)  =
    List.fold_left (fun acc e ->
      if acc = "" then Printf.sprintf "%c" e else Printf.sprintf "%s%c" acc e) "" l

let from_amp s =

  let rec reduce_from_amp sq = 
    match sq with 
    | '&'::'l'::'t'::';'::rest_of_string -> '<'::(reduce_from_amp rest_of_string)
    | '&'::'g'::'t'::';'::rest_of_string -> '>'::(reduce_from_amp rest_of_string)
    | '&'::'a'::'m'::'p'::';'::rest_of_string -> '&'::(reduce_from_amp rest_of_string)
    | z :: rest_of_string -> z::(reduce_from_amp rest_of_string)
    | [] -> []
  in
  (string_to_char_list s) |> reduce_from_amp |> char_list_to_string

(* string is everything between "" brackets *)
let string_p: string parser = {
  run = fun state -> 
   
    let curr_token = !( state.tokens ).( state.token_ix ) in
    let (line, column) = Lexer.token2pos curr_token in
    match curr_token with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_String(l_str, _, _) -> 
         {state with token_ix = (state.token_ix + 1); }, Ok (from_amp l_str)
    | token -> 
      state, Error ( sprintf ": String (Tok_String) expected but got: >%s< at: (line:%d, col:%d) " (token2str token) line column)
}



(* text is everyting between '>' and '<' characters *)
let text_p: xml_object parser = {
  run = fun state -> 
    let curr_token = !( state.tokens ).( state.token_ix ) in
    let (line, column) = Lexer.token2pos curr_token in
    match curr_token with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_Text (l_str, _, _) ->
          (* let pos = Str.search_forward (Str.regexp {|&\([^;]*\);|}) l_str 1 in  *)
         {state with token_ix = (state.token_ix + 1); }, Ok (Text_El (from_amp l_str))
    | token -> 
      state, Error ( sprintf ": Text data (Tok_Text) element expected but got: >%s< at: (line:%d, col:%d) " (token2str token) line column)
}

let attribute_p: (string * string) parser = 
  (name_p <* (is_a (Tok_Equ(0,0)))) <*> string_p >>= 
          fun (n, s) -> return (n,s)

let element_init_p : xml_object parser = 
  
  ((is_a (Tok_Less(0,0))) *> (name_p <*> zeroOrMore attribute_p) <* (is_a (Tok_More(0,0))) >>= 
    fun (n, l) -> return (Tag_El {name = n; childs = []; attributes = l})) 

let element_end_p : string parser = 
  (is_a (Tok_Less(0,0))) *> (is_a (Tok_Slash(0,0))) *> name_p <* (is_a (Tok_More(0,0))) >>= 
    fun (name) -> return name

let autoclose_element_p : xml_object parser =  
      ((is_a (Tok_Less(0,0))) *> (name_p <*> zeroOrMore attribute_p) <* (is_a (Tok_Slash(0,0))) <* (is_a (Tok_More(0,0))) <* zeroOrMore text_p >>= 
        fun (n, l) -> return (Tag_El {name = n; childs = []; attributes = l}))

let br_element_p : xml_object parser =  
  ((is_a (Tok_Less(0,0))) *> (keyword_p "br") <* (is_a (Tok_More(0,0))) <* zeroOrMore text_p >>= 
    fun _ -> return (Tag_El {name = "br"; childs = []; attributes = []}))

    (* recursive parser must have this form *)
let rec tag_element_p : xml_object parser = 
  { run = fun state -> state |> (
  element_init_p <*> (((zeroOrMore text_p) *> (oneOrMore xml_object_p)) <|> (oneOrMore text_p)) <*> element_end_p <* zeroOrMore text_p >>= 
    fun ((e, ch), end_tag) ->
      match (e, ch) with 
      | (Tag_El e1, ch) ->     
          let curr_token = !( state.tokens ).( state.token_ix ) in
          let (line, column) = Lexer.token2pos curr_token in
          e1.childs <- ch;
          if (String.compare e1.name end_tag) <> 0 then (fail (sprintf " ** Open tag:<%s> is different than close tag:</%s> at (line:%d, col:%d) " e1.name end_tag line column))
          else return (Tag_El e1)
      | (e2, _) -> return e2 
  ).run
}

(*
and xml_object_p : xml_object parser = {
  run = fun state -> 
    (match (state |> text_p.run) with
      | (state, Ok r) -> (state, Ok r)
      | _, Error _ ->  
        (match (state |> tag_element_p.run) with  (* don't care about error if have some alternative *)
            | (state, Ok r') -> (state, Ok r')
            | _, Error _ -> (state |> autoclose_element_p.run)
        )
    )
}
*)
and xml_object_p : xml_object parser = { run = fun state ->
  
  (br_element_p <|> autoclose_element_p <|> tag_element_p  (* <|> fail "<nieznany xml_obiekt>" *) ).run state }


let parser_run (p: 'a parser) (t: token array ref): ('a, error) result =

  match (t, 0, 0) |> set_state |> p.run with
  | _     , Ok x    -> Ok x
  | state', Error desc -> Error {token_ix = state'.token_ix; desc = desc; }

let rec pp (el : xml_object) (prefix : string) (curr_prefix : string) = 
  let prefix2 = sprintf "%s%s" prefix curr_prefix in
  let prefix3 = sprintf "%s%s%s" prefix prefix curr_prefix in
  match el with
    | Tag_El el0 ->
      let attributes_pp_l = (List.fold_left (fun n (s1,s2) -> sprintf "\n%s%s=\"%s\" %s" prefix3 s1 s2 n) "" el0.attributes) in
      let childs_pp_l = (List.fold_left (fun n ch -> sprintf "\n%s%s%s" prefix (pp ch prefix prefix2) n) "" el0.childs) in

      let attributes_pp = if String.length attributes_pp_l > 0 then sprintf "attributes:%s\n" attributes_pp_l else "" in
      let childs_pp = if String.length childs_pp_l > 0 then sprintf "%s\n" childs_pp_l else "" in

      sprintf "%s%s\n%s%s%s%s" curr_prefix el0.name prefix3 attributes_pp prefix2 childs_pp 

    | Text_El s -> sprintf "%stext:\"%s\"" curr_prefix s

let () = printf "\n****************************************************\nHello World\n"