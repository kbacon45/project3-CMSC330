open List
open Nfa
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference:

  type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t
*)

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let getFinal lst = 
  match lst with
  | [] -> -1
  | x::_ -> x

let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = 
  match regexp with
  | Empty_String -> 
      let one = fresh () in 
      { sigma = []; qs = [one]; q0 = one; fs = [one]; delta = [] }

  | Char char -> 
      let one = fresh () in 
      let two = fresh () in 
      { sigma = [char]; qs = [one; two]; q0 = one; fs = [two]; 
        delta = [{ input = Some char; states = (one, two) }] }

  | Union (r1, r2) -> 
      let newN = fresh () in 
      let newF = fresh () in 
      let s1 = regexp_to_nfa r1 in 
      let s2 = regexp_to_nfa r2 in 
      { sigma = (union s1.sigma s2.sigma); 
        qs = (union s1.qs s2.qs); 
        q0 = newN; 
        fs = [newF]; 
        delta = [{ input = None; states = (newN, s1.q0) }] @
                [{ input = None; states = (newN, s2.q0) }] @
                [{ input = None; states = (getFinal s1.fs, newF) }] @
                [{ input = None; states = (getFinal s2.fs, newF) }] @
                s1.delta @ s2.delta }

  | Concat (r1, r2) -> 
      let s1 = regexp_to_nfa r1 in 
      let s2 = regexp_to_nfa r2 in 
      { sigma = (union s1.sigma s2.sigma); 
        qs = (union s1.qs s2.qs); 
        q0 = s1.q0; 
        fs = [getFinal s2.fs]; 
        delta = [{ input = None; states = (getFinal s1.fs, s2.q0) }] @ 
                s1.delta @ s2.delta }

  | Star regex -> 
      let str = regexp_to_nfa regex in 
      { sigma = str.sigma; 
        qs = str.qs; 
        q0 = str.q0; 
        fs = str.fs; 
        delta = [{ input = None; states = (getFinal str.fs, str.q0) }] @
                [{ input = None; states = (str.q0, getFinal str.fs) }] @
                str.delta }


(* The following functions are useful for testing, we have implemented them for you *)
let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str