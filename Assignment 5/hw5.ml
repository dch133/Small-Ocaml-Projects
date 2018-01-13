(*David Gilbert   260746680
  Daniel Chernis  260707258
*)


module Exp =
struct
  type name   = string
  type primop = Equals | LessThan | Plus | Minus | Times | Negate

  type exp =
    | Var of name
    | Int of int                      (* 0 | 1 | 2 | ... *)
    | Bool of bool                    (* true | false *)
    | If of exp * exp * exp           (* if e then e1 else e2 *)
    | Primop of primop * exp list     (* e1 <op> e2  or  <op> e *)
    | Let of dec * exp                (* let dec in e end  <--- NEW!!!! *)
    | Pair of exp * exp               (* (e1, e2)   <--- NEW!!!! *)
    | Fst of exp                      (* fst e <--- NEW!!!! *)
    | Snd of exp                      (* snd e <--- NEW!!!!*)

    (*let a = Var "a"*)

  and dec =
    | Val of exp * name               (* x = e *)
    | Match of exp * name * name      (* x, y = e  <--- NEW!!!! *)


  (* ---------------------------------------------------------------- *)
  (* Generating new variable names *)

  let genCounter =
  let counter = ref 0 in
  ((fun x ->
    let _ = counter := !counter+1 in
    x ^ string_of_int (!counter)),
  fun () ->
    counter := 0)

  let (freshVar, resetCtr) = genCounter

 (* ---------------------------------------------------------------- *)
 (* Basic functions about lists *)

  let member x l = List.exists (fun y -> y = x) l

  let rec delete (vlist, l) = match l with
    |  [] -> []
    |  h :: t ->
       if member h vlist then delete (vlist, t)
       else h :: delete (vlist, t)

  let rec union p = match p with
  | ([], l) -> l
  | (x::t, l) ->
    if member x l then
      union (t, l)
    else
      x :: union (t, l)

  (* ---------------------------------------------------------------- *)
  (* Computing the set of free variables in an expression *)

  (* Q1.2: extend the function for Pair(_, _) and Let (Match(_, _, _), _) *)
  

  let rec freeVars e = match e with
  | Var y -> [y]
  | Int n -> []
  | Bool b -> []
  | If(e, e1, e2) ->
    union (freeVars e, union (freeVars e1, freeVars e2))
  | Primop (po, args) ->
    List.fold_right (fun e1 fv -> union (freeVars e1, fv)) args []
  | Let (Val (e1, x), e2) ->
      union (freeVars e1, delete ([x], freeVars e2))
  | Pair (e1, e2) -> union (freeVars e1, freeVars e2)
  | Let (Match (e1, x, y), e2) -> union (freeVars e1 , delete ([x;y], freeVars e2) )
  | Fst e -> freeVars e
  | Snd e -> freeVars e
  



  (* ---------------------------------------------------------------- *)
  (* Substitution
   subst : (exp * name) -> exp -> exp

   subst (e',x) e = [e'/x]e

   subst replaces every occurrence of the variable x
   in the expression e with e'.
  *)

  (* Q1.4: extend subst for Pair(_, _) and both Let (Match(_,_,_), _)  and Let (Val(_,_), _) *)
  let rec subst (e',x as s) exp =
    match exp with
    | Var y ->
       if x = y then e'
       else Var y
    | Int n  -> Int n
    | Bool b -> Bool b
    | Primop(po, args) ->
       Primop(po, List.map (subst s) args)
    | If(e, e1, e2) ->
       If(subst s e, subst s e1, subst s e2)
    | Let (Val(e1,y), e2) ->
       let e1' = subst s e1 in
       if x = y then
         (* optimization: don't traverse e2 as there is not free occurrence of x in e2 *)
         Let (Val (e1', y), e2)
       else
         if member y (freeVars e') then
           let y'  = freshVar y in
           let e2' = rename (y', y) e2 in
             Let(Val(e1', y'), subst s e2')
         else
           Let(Val(e1', y), subst s e2)
    | Let (Match (e1, x, y), e2) -> 
       let a  = freshVar x in      (*rename 'x' in the pair to avoid confusion*)
       let e1' = rename (a,x) e1 in 
        (*if both elements of pair = x OR 1st or 2nd elem of pair = x :*)
       if x = a && x = y || x = a || x = y then Let (Match (subst s e1', a, y), e2)    
       else (*check y is not bound in expression to be substituted*)
          if member y (freeVars e') then
          let y' = freshVar y in
          let e2' = rename (y', y) e2 in
            Let (Match(subst s e1', a, y), subst s e2')
          (*check a is not bound in expression to be substituted*)
          else if member a (freeVars e') then
          let a'  = freshVar a in
          let e2' = rename (a', a) e2 in
            Let (Match(subst s e1', a, y), subst s e2')
          (*check a and y are both not bound in expression to be substituted*)
          else if member a (freeVars e') && member y (freeVars e') then
          let a'  = freshVar a in
          let y'  = freshVar y in
          let e2' = rename (a', a) e2 in
          let e2'' = rename (y', y) e2' in
            Let (Match(subst s e1', a, y), subst s e2'')
          
          else Let (Match (subst s e1', a, y), subst s e2 )        
    | Pair (e1, e2) -> Pair( subst s e1, subst s e2)
    | Fst e ->  Fst (subst s e)
    | Snd e -> Snd (subst s e)

  and rename (x', x) e = subst (Var x', x) e
end

module Types =
  struct
    module E = Exp

    type tp = Int | Bool | Prod of tp * tp

    let rec typ_to_string t = match t with
      | Int -> "Int"
      | Bool -> "Bool"
      | Prod (t1, t2) -> typ_to_string t1 ^ " * " ^ typ_to_string t2

    exception TypeError of string

    let fail message = raise (TypeError message)

    type ctx = (E.name * tp ) list

    let lookup n g =
      try
        List.assoc n g
      with
        _ -> fail ("Could not find variable in the context")

    (* primopType p = (argTypes, returnType) *)
    let primopType p = match p with
      | E.Equals   -> ([Int; Int], Bool)
      | E.LessThan -> ([Int; Int], Bool)
      | E.Plus     -> ([Int; Int], Int)
      | E.Minus    -> ([Int; Int], Int)
      | E.Times    -> ([Int; Int], Int)
      | E.Negate   -> ([Int], Int)


    (* Q1.6: extend infer to support Pair(_, _) and Let (Match(_,_,_), _) *)

    let rec infer g e = match e with
      | E.Int _ -> Int
      | E.Bool _ -> Bool
      | E.If (e, e1, e2) ->
         (match infer g e with
         | Bool -> let t1 = infer g e1 in
                   let t2 = infer g e2 in
                   if t1 = t2 then t1
                   else fail ("Expected " ^ typ_to_string t1 ^
                              " - Inferred " ^ typ_to_string t2)
         | t -> fail ("Expected Bool\nInferred " ^ typ_to_string t))
      | E.Primop (po, args) ->
         let (expected_arg_types, resultType) = primopType po in
         let inferred_arg_types = List.map (infer g) args in

         let rec compare tlist1 tlist2 = match tlist1, tlist2 with
           | [] , [] -> resultType
           | t::tlist , s::slist ->
              if t = s then compare tlist slist
              else fail ("Expected " ^ typ_to_string t ^
                         " - Inferred " ^ typ_to_string s)
           | _ , _ -> fail ("Error: Primitve operator used with incorrect number of arguments")
         in
           compare expected_arg_types inferred_arg_types
      | E.Var x -> lookup x g
      | E.Let (E.Val (e1, x), e2) ->
         let t = infer g e1 in
         infer ((x, t)::g) e2
      | E.Pair (e1, e2) -> Prod (infer g e1, infer g e2)
      | E.Let (E.Match (e1, x, y), e2) -> 
         let t1 = lookup x g in (*deduce type of x-value with lookup func*)
         let t2 = lookup y g in (*deduce type of y*)
         (*deduce type of 'e1' given gamma, x::t1, y::t2*)
         infer ((x,t1)::(y,t2)::g) e2 (*deduce type of 'e2' given gamma, x:t1, y:t2*)
         
    end

module Eval =
  struct
    open Exp

    exception Stuck of string

    (* Q1.8: extend eval to support Pair(_, _) and Let (Match(_,_,_), _) *)

    let evalOp op = match op with
      | (Equals,   [Int i; Int i']) -> Some (Bool (i = i'))
      | (LessThan, [Int i; Int i']) -> Some (Bool (i < i'))
      | (Plus,     [Int i; Int i']) -> Some (Int (i + i'))
      | (Minus,    [Int i; Int i']) -> Some (Int (i - i'))
      | (Times,    [Int i; Int i']) -> Some (Int (i * i'))
      | (Negate,   [Int i])         -> Some (Int (-i))
      | _                           -> None

    let rec eval e = match e with
      | Int _ -> e
      | Bool _ -> e
      | If(e, e1, e2) ->
         (match eval e with
         | Bool true -> eval e1
         | Bool false -> eval e2
         | _ -> raise (Stuck "guard is not a bool"))
  (* primitive operations +, -, *, <, = *)
      | Primop (po, args) ->
         let argvalues = List.map eval args in
         (match evalOp (po, argvalues) with
         | None -> raise (Stuck "Bad arguments to primitive operation")
         | Some v -> v)
      | Let (Val (e1, x), e2) -> eval (subst (eval e1, x) e2)
      | Var _ -> raise (Stuck "Bug : we only evaluate closed terms")    (* Variables would not occur in the evaluation of closed terms *)
      | Pair (e1, e2) -> Pair (eval e1, eval e2)
      | Let (Match (e1, x, y), e2) -> 
        let ev = eval e1 in
        (match ev with (*check e1 is a pair else throw Exception*)
          | Pair (a,b) -> 
              let e2' = subst (a, x) e2 in
              (*replace 'x' into evaluated e1*)
              let e2'' = subst (b, y) e2' in (*replace 'y' into e1'*)
              eval e2'' (*eavluate the expression with substituted pair x,y*)
          |_ -> raise (Stuck "Expression is not a Pair")
        )
               
        
        
  end


module E = Exp
let e1 = E.If (E.Primop (E.Equals, [E.Int 3; E.Int 2]),
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))

let e2 = E.If (E.Primop (E.Equals, [E.Int 3; E.Bool true]),
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))
               
let e3 = E.Let (E.Val (E.Int 3, "x"), E.Primop (E.Plus, [E.Var "x" ; E.Int 2]))

let e4 = E.Let (E.Val (E.Int 3, "x"),
                E.Let (E.Val (E.Int 2, "y"), E.Primop (E.Plus, [E.Var "x" ; E.Var "y"])))

(* Question 2 : There’s more than one way to do it  *)

(* Q2.1 Extend on the definition of the free variables function with fst and snd. *)
(* Q2.2 Extend the definition of subst function with fst and snd. *)


module type Optimization =
  sig
    val optimize : E.exp -> E.exp
  end

(* Q3.1: implement dead code elimintion *)

module DeadCode : Optimization =
  struct
    open Exp
    let rec optimize exp = match exp with
      | If(e, e1, e2) -> If(optimize e, optimize e1, optimize e2)
      | Let(Val(e1, x), e2) -> 
          let e2' = optimize e2 in
          if member x (freeVars e2' ) then Let(Val(e1, x), e2')
          else e2'
      | Let (Match (e1, x, y), e2) -> 
          let e2' = optimize e2 in
          if member x (freeVars e2') || member y (freeVars e2') then 
            Let (Match (e1, x, y), e2')
          else e2' 
      | Primop (po, args) -> Primop (po, (List.map optimize args) )
      | _ -> exp      
  end

(* Q3.2: implement the elimination of pattern matching let *)

module RemoveLetMatch : Optimization =
  struct
    open Exp
    let rec optimize exp = match exp with
      | Pair (e1, e2) -> Pair (optimize e1, optimize e2)
      | Let (Match (e1, x, y), e2) -> 
        if member x (freeVars e2) || member y (freeVars e2) then  
          let e1' = optimize e1 in
            let e2' = subst (Fst e1', x) (optimize e2) in
            let e2'' = subst (Snd e1', y) (optimize e2') in 
            Let(Val(e1', "z"), e2'')   
        else Let(Val(optimize e1,"z"), optimize e2)    
      | Let(Val(e1, x), e2) -> 
          let e2' = optimize e2 in
          if member x (freeVars e2' ) then Let(Val(e1, x), e2')
          else e2'
      | If (e, e1,e2) -> If (optimize e, optimize e1, optimize e2)
      | Fst Pair(e1,e2) -> Fst (Pair(optimize e1, e2) )
      | Snd Pair(e1,e2) -> Snd (Pair(e1, optimize e2) )
      | Primop (po, args) -> Primop (po, (List.map optimize args) )
      | _ -> exp
  end

module Compose (M1 : Optimization) (M2 : Optimization) : Optimization =
  struct
    let optimize e = M1.optimize (M2.optimize e)
  end

(* To test one after the other use this pipeline *)
(* module Pipeline = Compose (DeadCode) (RemoveLetMatch) *)
(* Think about if the order in which you apply matters? is this always
   the case? is there always a good choice? *)


(*

let x,y = 5,7 in let z = 2 in x + y + z => 14

let x,y = 5,7 in let z = 2 in y +z => 9

let x,y = 5,7 in let z = 2 in x + y  => 12

*)

let plus e1 e2 =
  Exp.Primop(Exp.Plus, [e1 ; e2])

(* These lines use a local opening of modules, check this URL to see the explanation:
   https://realworldocaml.org/v1/en/html/files-modules-and-programs.html
*)
let e5 = let open Exp in Let (Match (Pair (Int 5, Int 7), "x", "y"), Let (Val (Int 2, "z"), plus (Var "x") (plus (Var "y") (Var "z"))))
let e6 = let open Exp in Let (Match (Pair (Int 5, Int 7), "x", "y"), Let (Val (Int 2, "z"), plus (Var "y") (Var "z")))
let e7 = let open Exp in Let (Match (Pair (Int 5, Int 7), "x", "y"), Let (Val (Int 2, "z"), plus (Var "x") (Var "y")))


let e8 = let open Exp in Let (Val (Int 3, "z"), Let (Val (Int 7, "x"), Let (Val (Var "x", "y"), Var "z")))
