open Regex_base

let rec repeat n l =
  if n <= 0 then []
  else l @ repeat (n - 1) l
;;

let rec expr_repeat n e =
  let rec aux n e =
    if n = 2 then Concat (e,e)
    else Concat (e, aux (n-1) e)
  in 
  if n = 1 then e
  else if n = 0 then Eps
  else aux n e
;;

let rec is_empty e =
  match e with
  | Eps -> true
  | Base x -> false
  | Concat (x,y) -> is_empty x && is_empty y
  | Alt (x,y) -> is_empty x && is_empty y
  | Star x -> is_empty x
  | Joker -> false
;;

let rec null e =
  match e with
  | Eps -> true
  | Base x -> false
  | Concat (x,y) ->  null x && null y
  | Alt (x,y) ->  null x || null y
  | Star x -> true
  | Joker -> false
;;

let rec is_finite e =
  let rec is_only_eps expr =
  match expr with
  | Eps -> true
  | Base _ -> false
  | Joker -> false
  | Concat (e1, e2) -> is_only_eps e1 && is_only_eps e2
  | Alt (e1, e2) -> is_only_eps e1 && is_only_eps e2
  | Star e -> is_only_eps e
  in
  let rec aux e onlyEps =
    match e with
    | Eps -> true
    | Base x -> true
    | Concat (x,y) -> aux x (is_only_eps x) && aux y (is_only_eps y)
    | Alt (x,y) -> aux x (is_only_eps x) && aux y (is_only_eps y)
    | Star x -> onlyEps 
    | Joker -> true
  in
  aux e (is_only_eps e)
;;

let rec product l1 l2 =
  match l1 with
  | [] -> []
  | y::reste ->
     let x l3 = y@l3
     in
     (List.map x l2) @ (product reste l2)   
;;

let enumerate (alphabet:char list) (e:char expr) : char list list option =
  let char_list_to_char_list_list (lst : char list) : char list list =
    List.map (fun c -> [c]) lst
  in
  match e with
  | Eps -> Some [[]]
  | Base x -> Some [[x]]
  | Concat (x,y) ->
     (match x with
     | Base z -> 
        (match y with
          | Joker ->  Some (product [[z]] (char_list_to_char_list_list alphabet))
          | Eps -> None 
          | Base z -> None
          | Concat (x,y) -> None 
          | Alt (x,y) -> None
          | Star (x) -> None)
     | Eps -> None 
     | Joker -> None
     | Concat (x,y) -> None 
     | Alt (x,y) -> None
     | Star (x) -> None)
  | Alt (x,y) -> None
  | Star x -> None 
  | Joker -> Some [[]]
;;

let rec alphabet_expr e =
  let rec aux e1 =
  match e1 with
  | Eps -> []
  | Base x -> [x]
  | Concat (x,y) -> aux x @ aux y
  | Alt (x,y) -> aux x @ aux y
  | Star x -> aux x
  | Joker -> []
  in List.sort_uniq compare (aux e)
;;

type answer =
  Infinite | Accept | Reject

let accept_partial (e : char expr) (w : char list) : answer = 
  let rec aux e w index = 
    match e with
    | Eps -> 
      if index >= List.length w then true
      else false
    | Base a -> 
      if not(index >= List.length w) && w<>[] && (List.nth w index) = a then true
      else false
    | Joker -> true
    | Concat (a,b) -> 
      aux a w (index) && aux b w (index+1)
    | Alt (a,b) ->
      aux a w (index) || aux b w (index)
    | Star a -> false
  in 
  if(is_finite e) then
    if aux e w 0 then Accept
    else Reject
  else Infinite
;;
