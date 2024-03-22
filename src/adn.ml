type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec explode_aux str index result = 
    if index < 0 then result
    else explode_aux str (index-1) (str.[index] :: result)
  in explode_aux str ((String.length str)-1) []

(* conversions *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _ -> WC


let dna_of_string (s : string) : base list =
  let rec dna_aux stab =
    match stab with
    | [] -> []
    | tete :: reste -> (base_of_char tete) :: dna_aux reste
  in dna_aux (explode s);;

let string_of_dna (seq : dna) : string =
  let rec string_of_dna_aux seq = 
    match seq with
    | [] -> ""
    | tete :: reste -> string_of_base tete ^ (string_of_dna_aux reste)
  in string_of_dna_aux seq;;


(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec cut_aux slice list =
    (match slice with
    | [] -> Some list
    | tete :: reste -> 
      (match list with
      | [] -> None
      | x :: y -> 
        if tete=x then cut_aux reste y
        else None))
  in cut_aux slice list

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  let list_option_to_list opt=
    match opt with
    | None -> []   
    | Some lst -> lst
  in
  let rec first_aux slice list before =
    match list with
    | [] -> 
      if slice=[] then Some(before, [])
      else None
    | tete :: reste ->
      let pre = cut_prefix slice list in
      if not(pre = None) then Some (before, (list_option_to_list pre))
      else first_aux slice reste (before@[tete])
  in first_aux slice list [];;

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  match list with
  | [] -> []
  | tete :: reste ->
    let sous_lst = cut_prefix start list in
      match sous_lst with
      | None -> slices_between start stop reste
      | Some(suff) -> 
        let sous_lst2 = first_occ stop suff in
          match sous_lst2 with 
          | None -> []
          | Some(pre2,suff2) -> pre2::(slices_between start stop suff2)
;;

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A;T;G] [T;A;A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus =
  let rec remove list a =
    match list with
    | [] -> []
    | tete :: reste -> 
      if tete=a then remove reste a 
      else tete :: remove reste a
  in
  let max_aux cpt b = 
    match cpt with
    | (a, b2) ->
      if a > b then (a, b2)
      else if a < b then (b, true)
      else (a, false) 
  in
  let rec compte_value (list : 'a list) (value : 'a) result : int =
    match list with
    | [] -> result
    | tete :: reste -> 
      if tete=value then compte_value reste value (result+1)
      else compte_value reste value result
  in let rec consensus_aux (list2 : 'a list) valeur compteur : 'a consensus =
      match list2 with
      | [] -> 
        (match compteur with
        | (a, b) ->
          if not(b) then No_consensus
          else if a = List.length list then 
            match valeur with
            | None -> No_consensus
            | Some valeur2 -> Full (valeur2)
          else 
            match valeur with
            | None -> No_consensus
            | Some valeur2 -> Partial (valeur2, a)
            )
      | tete :: reste -> 
        let cpt = compte_value list2 tete 0 in
        let list_remove_valeur = remove list2 tete in
        let max = max_aux compteur cpt in
        let new_val = 
          if max<>compteur then Some tete
          else valeur in
          consensus_aux list_remove_valeur new_val max
  in (consensus_aux list None (-1, false));;
(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  let rec construit_list_index listdouble index =
    match listdouble with
    | [] -> []
    | tete :: reste -> 
      (List.nth tete index) :: (construit_list_index reste index)
  in let rec consensus_aux ll index = 
    if ll = [] then []
    else if index = List.length (List.hd ll) then []
    else consensus (construit_list_index ll index) :: consensus_aux ll (index+1)
  in consensus_aux ll 0;;



(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
