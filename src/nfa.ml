open List
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference

  type ('q, 's) transition = {
    input: 's option; 
    states: 'q * 'q;
  }

  (** NFA type *)
  type ('q, 's) nfa_t = {
    sigma: 's list;
    qs: 'q list;
    q0: 'q;
    fs: 'q list;
    delta: ('q, 's) transition list;
  } 
*)

(****************)
(* Part 1: NFAs *)
(****************)

let firstElem tup = match tup with
(a,b) -> a
let secondElem tup = match tup with
(a,b) -> b

let rec findTrans q sym delList = match delList with
[] -> []
| elem::rest -> if elem.input = sym && (firstElem elem.states) = q
  then (secondElem elem.states)::(findTrans q sym rest)
else findTrans q sym rest;;


let rec move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match qs with
[] -> []
| elem::rest -> let res = (findTrans elem s nfa.delta) in 
 match res  with
[] ->  (move nfa rest s)
| elem::emptyLst -> res@(move nfa rest s)


let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = match qs with
[] -> []
| elem::rest -> qs@(e_closure nfa (diff (move nfa qs None) qs))
  

let rec acceptHelp (nfa: ('q,char) nfa_t) lst states = match lst with
[] -> (let finalStates = (intersection states nfa.fs) in 
match finalStates with
[] -> false 
| curr::rest -> true )
| elem::rest2 -> let newStates = move  nfa (e_closure nfa states) elem in
 acceptHelp nfa rest2 newStates

let optConvert lst = List.map (fun x -> (Some x)) lst 
  
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let stringLst = (optConvert (explode s)) in acceptHelp nfa stringLst [nfa.q0]


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = List.fold_left (fun acc elem -> 
  acc@[(e_closure nfa (move nfa qs (Some elem)))]) [] nfa.sigma 



let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = List.fold_left (fun acc elem -> 
  acc@[{input=(Some elem); states=(qs,(e_closure nfa (move nfa qs (Some elem))))}]) [] nfa.sigma 

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = match (intersection qs nfa.fs) with
[] -> []
| x::xs -> [qs]

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "this function is optional"

(*let nfahelper (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t  =*)


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = failwith "unimplemented"


(*let newNFA = {sigma=nfa.sigma;qs=nfa.qs;q0=(e_closure nfa.qs);delta=nfa.delta} in
nfaHelper newNFA*)