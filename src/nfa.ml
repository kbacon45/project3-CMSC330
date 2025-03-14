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



let rec findTrans q sym delList repeats = match delList with
  | [] -> []
  | elemR :: rest -> 
      if elemR.input = sym && (firstElem elemR.states) = q then
        let newE = secondElem elemR.states in
        if elem newE repeats then 
          findTrans q sym rest repeats
        else 
          newE :: (findTrans q sym rest (newE :: repeats))
      else 
        findTrans q sym rest repeats





let rec move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match qs with
[] -> []
| elem::rest -> let res = (findTrans elem s nfa.delta []) in 
 match res  with
[] ->  (move nfa rest s)
| elem::emptyLst -> (union res (move nfa rest s) )


let rec e_closureHelp (nfa: ('q, 's) nfa_t) (qs: 'q list) (visited: 'q list) : 'q list =
  match qs with
  | [] -> visited  
  | elem::rest ->
      let new_states = (diff (move nfa [elem] None) visited) in
      (e_closureHelp nfa (rest @ new_states) (union visited new_states))

let e_closure (nfa: ('q, 's) nfa_t) (qs: 'q list) : 'q list =
  e_closureHelp nfa qs qs
  

let rec acceptHelp (nfa: ('q,char) nfa_t) lst states = match lst with
[] -> (let finalStates = (intersection (e_closure nfa states) nfa.fs) in 
match finalStates with
[] -> false 
| curr::rest -> true )
| elem::rest2 -> let newStates = move  nfa (e_closure nfa states) elem in if (newStates = []) then false else
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

let rec lookTrans (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (tBuildLst: ('q list, 's) transition list) : ('q list, 's) nfa_t = 
  match tBuildLst with
  | [] -> dfa
  | curr::rest -> 
      let goTo = secondElem curr.states in 
      if goTo <> [] then (
      if elem goTo dfa.qs 
      then lookTrans nfa {sigma = dfa.sigma; qs = dfa.qs; q0 = dfa.q0; fs = dfa.fs; delta = curr::dfa.delta } rest
      else lookTrans nfa {sigma = dfa.sigma; qs = goTo::dfa.qs; q0 = dfa.q0; fs = dfa.fs; delta = curr::dfa.delta } rest )
    else lookTrans nfa {sigma = dfa.sigma; qs = dfa.qs; q0 = dfa.q0; fs = dfa.fs; delta = dfa.delta } rest

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) (visited: 'q list list) : ('q list, 's) nfa_t = 
  match work with
  | [] -> dfa
  | x::xs -> 
      let tBuildLst = new_trans nfa x in 
      let newDfa = (lookTrans nfa dfa tBuildLst) in
      let newVisted = (union [x] visited) in
      nfa_to_dfa_step nfa newDfa (diff newDfa.qs newVisted) newVisted

      let rec addFinals (nfa: ('q, 's) nfa_t) (dfa: ('q list, 's) nfa_t) (stateLst: 'q list list) : ('q list, 's) nfa_t = 
        match stateLst with
        | [] -> dfa
        | curr::rest ->  
            if intersection curr nfa.fs <> [] then
              addFinals nfa {sigma = dfa.sigma; qs = dfa.qs; q0 = dfa.q0; fs = curr::dfa.fs; delta = dfa.delta } rest
            else
              addFinals nfa dfa rest
      
      
      

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let startStates = e_closure nfa [nfa.q0] in
  let dfaRes = nfa_to_dfa_step nfa {sigma = nfa.sigma; qs = [startStates]; q0 = startStates; fs = []; delta = []} [startStates] [startStates] in
  addFinals nfa dfaRes dfaRes.qs


