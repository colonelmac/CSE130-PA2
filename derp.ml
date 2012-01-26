#use "misc.ml"

let x = (10,2,[])
let y = (2,"derrick",[("derrick", 1)])

(*
let first = (assoc x)
let second = (assoc y)
*)

let printer str =
    Printf.printf "%d" str;;

let _ = printer (assoc y)
