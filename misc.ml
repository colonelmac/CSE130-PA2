(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* Problem 1a 
 *
 * params: this function expects a tuple 
 *         the first item should be the default return if nothing is found.
 *         the second item should be the target.
 *         the third item should be a list contain tuples to search through.
 *
 * This function uses the pattern matching capabilities of OCaml to 
 * search through a list of tuples for a specific value.
 * It uses tail call optimization by eliminating local variables and
 * any operations following the recursive call.
 *)

let rec assoc dkl = 
    match dkl with (d, k, l) ->
        if l = [] then d else 
            match l with h::t -> 
                match h with (a, b) ->
                    if a = k then b 
                    else 
                        assoc (d, k, t);;
 
(* Problem 1b *) 
 
let removeDuplicates l = 
  let rec helper (seen, rest) = 
      match rest with 
      | [] -> seen
      | h::t -> 
        let seen' = 
            if (List.mem h seen) = false then seen@[h] else seen
        in
        let rest' = t in 
	  
      helper (seen', rest') 
  in
      (helper ([],l))


(* Problem 1c *)

let rec wwhile (f, b) =

    let tmp = (f b) in 
        match tmp with
        | (x, y) -> 
            if y = false then x
            else
                wwhile (f, x);;


let fixpoint (f, b) = 
    wwhile (( fun x -> let b = (f x) in (b, b != x)), b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
