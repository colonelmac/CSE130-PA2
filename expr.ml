(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

type expr = 
    VarX
  | VarY
  | Sine       of expr
  | Cosine     of expr
  | Average    of expr * expr
  | Times      of expr * expr
  | Thresh     of expr * expr * expr * expr 
  | FunnyTimes of expr * expr * expr
  | Sqr        of expr

let rec exprToString e = 
    
    let ex = exprToString in
    match e with
    | VarX -> "x"
    | VarY -> "y"  
    | Sine(t) -> ("sin(pi*"^(ex t)^")")
    | Cosine(t) -> ("cos(pi*"^(ex t)^")")
    | Average(s, t) -> ("(("^(ex s)^"+"^(ex t)^")/2)" )
    | Times(s, t) -> ((ex s)^"*"^(ex t))
    | Thresh(s, t, u, v) -> ("("^(ex s)^"<"^(ex t)^"?"^(ex u)^":"^(ex v)^")")
    | FunnyTimes(s, t, u) -> ("(floor "^(ex s)^"* ceil "^(ex t)^"*"^(ex u)^")")
    | Sqr(s) -> ("("^(ex s)^"*"^(ex s)^")")
    

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)

let buildFunnyTimes(e1, e2, e3)       = FunnyTimes(e1, e2, e3)
let buildSqr(e1)                  = Sqr(e1)

let pi = 4.0 *. atan 1.0

let rec eval (e, x, y) = 

    match e with
    | Sine(t) -> sin (eval (t, x, y))
    | Cosine(t) -> cos (eval (t, x, y))
    | Average(s, t) -> (((eval (s, x, y)) +. (eval (t, x, y))) /. 2.0)
    | Times(s, t) -> ((eval (s, x, y)) *. (eval (t, x, y)))
    | Thresh(s, t, u, v) -> if ((eval (s, x, y)) < (eval (t, x, y))) then (eval (u, x, y)) else (eval (v, x, y))
    | VarX -> x
    | VarY -> y
    | FunnyTimes(s, t, u) -> ((floor (eval (s, x, y))) *. ((ceil (eval (t, x, y))) *. (eval (u, x, y))))
    | Sqr(s) -> (let e = (eval (s, x, y)) in (e *. e))
;;

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
