type token = 
  | Tok_Less
  | Tok_More
  | Tok_Equ
  | Tok_Slash
  | Tok_Word of string   (*  abc2  *)
  | Tok_String of string (* "abc" *)
  | Tok_Text of string (* >abc...< *)
  | Tok_End
  | Tok_NewL

exception TokenizerError of string

  (* lexer *)
let tokenize str = 
  
  let rec f pos s = 
  (* Printf.printf "tokenize str: %d -> %s" pos s; *)
  if pos >= String.length s then [Tok_End]
  else
    if (Str.string_match (Str.regexp "\\/") s pos) then begin
      (* Printf.printf "tokenize div\n"; *)
      Tok_Slash::(f (pos + 1) s)
    end   
    else if (Str.string_match (Str.regexp {|>\([^<]*\)<|}) s pos) then begin
      let token = Str.matched_group 1 s in
      let token_l = String.to_seq token |> Seq.filter (fun a -> a != '\n'&& a != '\t' && a != ' ' ) in
      (* Printf.printf "tokenize string: %s\n" token; *)
      let t0 = if Seq.length token_l > 0
        then Tok_More::(Tok_Text token)::Tok_Less::[] 
        else Tok_More::Tok_Less::[] in  (*we don't need empty text tokens here*)
      t0 @ (f (pos + (String.length token + 2)) s)
    end    
    else if (Str.string_match (Str.regexp "<") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_Less::(f (pos + 1) s)
    end    
    else if (Str.string_match (Str.regexp ">") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_More::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "=") s pos) then begin
      (* Printf.printf "tokenize equ\n"; *)
      Tok_Equ::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp {|\"\([^\"]*\)\"|}) s pos) then begin
      let token = Str.matched_group 1 s in
      (* Printf.printf "tokenize string: %s\n" token; *)
      (Tok_String token)::(f (pos + (String.length token + 2)) s)
    end
    else if (Str.string_match (Str.regexp "[a-zA-Z_@][a-zA-Z_@0-9]*") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize word: %s\n" token; *)
      (Tok_Word token)::(f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp {|\([\t ]+\)\|\(\;[^\n]*\)|}) s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize white: %s\n" token; *)
      (f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "[\n]+") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize newl: %s\n" token; *)
      Tok_NewL::(f (pos + (String.length token)) s)
    end 
    else
      raise (TokenizerError (Printf.sprintf "\n\tERR00 >> Nobody expects the string: [%s] (or spanish inquisition).\n" (Str.last_chars s ((String.length s) - pos))))
  in
  (f 0 str)

  let token2str (t: token) : string =

    match t with
      | Tok_Less -> "(<)"
      | Tok_More -> "(>)"
      | Tok_Equ -> "(=)"
      | Tok_Slash -> "(\\)"
      | Tok_Word s -> Printf.sprintf "(%s)" s  
      | Tok_String s -> Printf.sprintf "(\"%s\")" s  
      | Tok_Text s -> Printf.sprintf "_(%s)_" s  
      | Tok_NewL -> "(\\n)"
      | Tok_End -> "(Tok_End)"
  
  
  let rec tokensl2str (tokens: token list) : string =
  
    match tokens with
    | [] -> ""
    | s::xs ->  Printf.sprintf " %s\n%s" (token2str s) (tokensl2str xs)
  
  
  let tokens2str (tokens: token array ref) : string =
  
    Array.fold_left (fun acc t -> Printf.sprintf " %s\n %s" acc (token2str t)) "" !tokens
  
    
let showTokens (a : token array ref) =
Printf.printf "Tokens: %s\n" (tokens2str a)


let tokenCompare (t1: token) (t2: token) (strict: bool) : bool =

  match (t1, t2) with
  | Tok_Word s1, Tok_Word s2 -> if(strict) then (String.compare s1 s2) = 0 else true 
  | Tok_String s1, Tok_String s2 -> if(strict) then (String.compare s1 s2) = 0 else true
  | Tok_Text s1, Tok_Text s2 -> if(strict) then (String.compare s1 s2) = 0 else true
  | a,b -> a = b

