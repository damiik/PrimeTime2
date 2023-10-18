(* Copyright by Dariusz Mikołajczyk 2024 *)

type token = 
  | Tok_Less of int * int
  | Tok_More of int * int
  | Tok_Equ of int * int
  | Tok_Slash of int * int
  | Tok_Word of string * int * int   (*  abc2  *)
  | Tok_String of string * int * int (* "abc" or 'abc' *)
  | Tok_Text of string * int * int (* >abc...< *)
  | Tok_End of int * int
  | Tok_NewL of int * int

exception TokenizerError of string

  (* lexer *)
let tokenize s = 
  let string_length = String.length s in 
  let rec f pos ln col = 
  (* Printf.printf "tokenize str: %d -> %s" pos s; *)
  if pos >= string_length then [Tok_End (ln, col)]
  else
    if (Str.string_match (Str.regexp "\\/") s pos) then begin
      (* Printf.printf "tokenize div\n"; *)
      Tok_Slash(ln, col)::(f (pos + 1) ln (col + 1))
    end 
    else if (Str.string_match (Str.regexp "\n+") s pos) then begin
      let token_l = Str.matched_string s |> String.length in
      Dream.log "********** tokenize newl:%s*********\n" (Str.matched_string s);
      (f (pos + token_l) (ln + token_l) token_l)
    end 
    else if (Str.string_match (Str.regexp "[\x09\x20 ]+") s pos) then begin (* whitespaces *)
      let token_l = Str.matched_string s |> String.length in
      Dream.log "********** tokenize white:%s********\n" (Str.matched_string  s);
      (f (pos + token_l) ln (col + token_l))
    end
    else if (Str.string_match (Str.regexp ">\\([^<\n]+\\)[<\n]+") s pos) then begin (*  >...<  *)
      let token = Str.matched_group 1 s in
      (* let token_l = String.to_seq token |> Seq.filter (fun a -> a != '\n' && a != '\t' && != ' ') in *)
      let new_lines = String.to_seq token |> Seq.filter(fun a -> a == '\n') |> Seq.length in
      Dream.log "tokenize string2: %s***********\n" token;
      let token_len = String.length token in
      (* let t0 = if Seq.length token_l > 0 *)
      let t0 = if token_len > 0
        then Tok_More(ln, col)::(Tok_Text (token, ln, col + 1))::Tok_Less(ln, col + 1 + token_len)::[] 
        else Tok_More(ln, col)::Tok_Less(ln, col + token_len)::[] in  (*we don't need empty text tokens here*)

      if(new_lines > 0) then t0 @ (f (pos + token_len + 2) (ln + new_lines) 1)
      else 
        t0 @ (f (pos + token_len + 2) ln (col + token_len + 2))
    end    
    else if (Str.string_match (Str.regexp "<") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_Less(ln, col)::(f (pos + 1) ln (col + 1))
    end    
    else if (Str.string_match (Str.regexp ">") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_More(ln, col)::(f (pos + 1) ln (col + 1))
    end
    else if (Str.string_match (Str.regexp "=") s pos) then begin
      (* Printf.printf "tokenize equ\n"; *)
      Tok_Equ(ln, col)::(f (pos + 1) ln (col + 1))
    end
    else if (Str.string_match (Str.regexp {|\"\([^\"]*\)\"|}) s pos) then begin
      let token = Str.matched_group 1 s in
       Dream.log "*********** tokenize string: %s\n" token; 
      (Tok_String (token, ln, col))::(f (pos + 2 + String.length token) ln (col + 2 + String.length token))
    end
    else if (Str.string_match (Str.regexp {|\'\([^\']*\)\'|}) s pos) then begin
      let token = Str.matched_group 1 s in
      (* Printf.printf "tokenize string: %s\n" token; *)
      (Tok_String (token, ln, col))::(f (pos + 2 + String.length token) ln (col + 2 + String.length token))
    end
    else if (Str.string_match (Str.regexp {|[a-zA-Z_][a-zA-Z_0-9\-]*|}) s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize word: %s\n" token; *)
      (Tok_Word (token, ln, col))::(f (pos + String.length token) ln (col + String.length token))
    end
    else
      raise (TokenizerError (Printf.sprintf "XML TOKENIZER ERROR>> Nobody expects the string: [%s] (or spanish inquisition).\n" (Str.last_chars s ((String.length s) - pos))))
  in
  (f 0 1 1)

  let token2str (t: token) : string =

    match t with
      | Tok_Less(_,_) -> "Tok_Less(<)"
      | Tok_More(_,_) -> "Tok_More(>)"
      | Tok_Equ(_,_) -> "Tok_Equ(=)"
      | Tok_Slash(_,_) -> "Tok_Slash(/)"
      | Tok_Word (s,_,_) -> Printf.sprintf "Tok_Word(%s)" s  
      | Tok_String (s,_,_) -> Printf.sprintf "Tok_String(\"%s\")" s  
      | Tok_Text (s,_,_) -> Printf.sprintf "Tok_Text(%s)" s  
      | Tok_NewL(_,_) -> "Tok_NewL()"
      | Tok_End(_,_) -> "Tok_End()"

let token2pos (t: token) : int*int =

        match t with
          | Tok_Less(l,c) -> (l,c)
          | Tok_More(l,c) -> (l,c)
          | Tok_Equ(l,c) -> (l,c)
          | Tok_Slash(l,c) -> (l,c)
          | Tok_Word (_, l, c) -> (l,c) 
          | Tok_String (_,l,c) -> (l,c)
          | Tok_Text (_,l,c) -> (l,c)
          | Tok_NewL(l,c) -> (l,c)
          | Tok_End(l,c) -> (l,c)
  
  let rec tokensl2str (tokens: token list) : string =
  
    match tokens with
    | [] -> ""
    | s::xs -> 
      let (ln, col) = token2pos s in
      Printf.sprintf " %s (%d, %d)\n%s" (token2str s) ln col (tokensl2str xs)
  
  
  let tokens2str (tokens: token array ref) : string =
  
    Array.fold_left (fun acc t -> Printf.sprintf " %s\n %s" acc (token2str t)) "" !tokens
  
    
let showTokens (a : token array ref) =
Printf.printf "Tokens: %s\n" (tokens2str a)


let tokenCompare (t1: token) (t2: token) (strict: bool) : bool =

  match (t1, t2) with
  | Tok_Word (s1, _, _), Tok_Word (s2, _, _) -> if(strict) then (String.compare s1 s2) = 0 else true 
  | Tok_String (s1, _, _), Tok_String (s2, _, _) -> if(strict) then (String.compare s1 s2) = 0 else true
  | Tok_Text (s1, _, _), Tok_Text (s2, _, _) -> if(strict) then (String.compare s1 s2) = 0 else true
  | Tok_End (_, _), Tok_End (_, _) -> true
  | Tok_Equ (_, _), Tok_Equ (_, _) -> true
  | Tok_Less(_, _), Tok_Less(_, _) -> true
  | Tok_More(_, _), Tok_More(_, _) -> true
  | Tok_NewL(_, _), Tok_NewL(_, _) -> true
  | Tok_Slash(_, _), Tok_Slash(_, _) -> true
  | _, _ -> false


