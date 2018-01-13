(*Daniel Chernis and David Gilbert*)
(* 260707258           260746680 *)


(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)

let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a =  match t with
  |Node (a,[]) -> if p a then a else raise BackTrack
  |Node (a, [h]) -> if p a then a 
                  else (try find_e p h with BackTrack -> raise BackTrack)
  |Node (a, Node (h, t) :: (hs::ts) ) -> if p a then a 
                                        else (try find_e p (Node (h,t) ) 
                                          with BackTrack -> 
                                            find_e p (Node (h,hs::ts)) )  


(* Q1.1: write this function and it helper functions *)
let find (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = 
  try Some (find_e p t) with BackTrack -> None


  (* Find with failure continuations *)
let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option = 
  match t with
  |Node (a,[]) -> if p a then Some a else k ()
  |Node (a, [h]) -> if p a then Some a else find_k p h k 
  |Node (a, Node (h, t) :: (hs::ts) ) -> 
    if p a then Some a 
    else find_k p (Node (h,t)) (fun () -> 
                      (find_k p hs (fun () -> find_k p (Node (h,hs::ts) ) k ))) 
                                         
  


(* Q1.2: write this function and it helper functions *)
let find' (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = 
  find_k p t (fun () -> None)

(* Find all with continuations *)

let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b = 
  match t with
  |Node (a,[]) -> if p a then k [a] else k []
  |Node (a, [h]) -> if p a then find_all_k p h (fun el -> k (a::el) )
                    else find_all_k p h (fun el -> k el )
  |Node (a, Node (h, t) :: (hs::ts) ) ->
                if p a then find_all_k p (Node (h,t)) (fun el -> 
                  find_all_k p (Node (h,hs::ts)) (fun er -> k (el @ [a] @ er)))
                else find_all_k p (Node (h,t)) (fun el -> 
                        find_all_k p (Node (h,hs::ts)) (fun er -> k (el @ er))) 
                                         

(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun el -> match el with [] -> [] | a -> a )

(*find (fun x -> x =4) example*)
(*find' (fun x -> x = 16) example*)
(*find_all (fun x -> x > 15) example*)
(* An example to use *)

let example = Node (7, [ Node (1, [])
                         ; Node (2, [Node (16, [Node (18, [])])])
                         ; Node (4, [])
                         ; Node (9, [])
                         ; Node (11, [])
                         ; Node (15, [])
                         ])

let is_big x =  x > 10


(* Q2 : Rational Numbers Two Ways *)

type fraction = int * int

module type Arith =
  sig
    type t
    val epsilon : t             (* A suitable tiny value, like epsilon_float for floats *)

    val plus : t -> t -> t      (* Addition *)
    val minus : t -> t -> t     (* Substraction *)
    val prod : t -> t -> t      (* Multiplication *)
    val div : t -> t -> t       (* Division *)
    val abs : t -> t            (* Absolute value *)
    val lt : t -> t -> bool     (* < *)
    val le : t -> t -> bool     (* <= *)
    val gt : t -> t -> bool     (* > *)
    val ge : t -> t -> bool     (* >= *)
    val eq : t -> t -> bool     (* = *)
    val from_fraction : fraction -> t (* conversion from a fraction type *)
    val to_string : t -> string        (* generate a string *)
  end

module FloatArith : Arith =
struct
  type t = float
  let epsilon = epsilon_float
  let from_fraction (num, den) = float_of_int num /. float_of_int den

  let plus = (+.)
  let minus = (-.)
  let prod = ( *. )
  let div = ( /. )
  let abs = abs_float
  let lt = (<)
  let le = (<=)
  let gt = (>)
  let ge = (>=)
  let eq = (=)
  let to_string x = string_of_float x
end

(* Q2.1: Implement the Arith module using rational numbers (t = fraction) *)

module FractionArith : Arith = 
  struct
    type t = fraction
    let epsilon : t = (1,1000000)
    let from_fraction (n,d) = (n,d)

    let plus (n1,d1) (n2,d2)  = ( (n1*d2 + n2*d1) , (d1*d2) )     
    let minus (n1,d1) (n2,d2) = ( (n1*d2 - n2*d1) , (d1*d2) )   
    let prod (n1,d1) (n2,d2)  = ( (n1*n2), (d1*d2) )      
    let div (n1,d1) (n2,d2)   = ( (n1*d2), (d1*n2) )   
    let abs (n,d)             = (abs n, abs d)            
    let lt (n1,d1) (n2,d2)    = (n1*d2 < n2*d1)
    let le (n1,d1) (n2,d2)    = (n1*d2 <= n2*d1)
    let gt (n1,d1) (n2,d2)    = (n1*d2 > n2*d1)
    let ge (n1,d1) (n2,d2)    = (n1*d2 >= n2*d1)
    let eq (n1,d1) (n2,d2)    = (n1*d2 = n2*d1)
    let to_string (n,d)       = string_of_int n ^"/"^string_of_int d   
  end

module type NewtonSolver =
  sig
    type t
    val square_root : t -> t
  end
  
(*let f1 = FractionArith.from_fraction((1,2):fraction)
 let f2 = FractionArith.from_fraction((3,4):fraction)
 
 FractionArith.epsilon |> FractionArith.to_string
FractionArith.to_string f1
*)

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)

module Newton (A : Arith) : (NewtonSolver with type t = A.t) = 
  struct 
    type t = A.t
    let square_root a = 
      let rec findroot x acc = 
        let x' = (A.prod (A.plus (A.div a x) x) (A.from_fraction ((1,2):fraction))) in
        if A.lt (A.abs (A.minus x x')) acc then x                
        else findroot x' acc  
      in findroot a (A.epsilon)
  end 

(* Examples *)

(* module FloatNewton = Newton (FloatArith) *)
(* module RationalNewton = Newton (FractionArith) *)

(* let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (2, 1)) *)
(* let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (2, 1)) *)
(*FloatArith.to_string sqrt2*)
(*FractionArith.to_string sqrt2_r*)

(* Q3 : Real Real Numbers, for Real! *)

type 'a stream = { head : 'a  ; tail : unit -> 'a stream}

let rec nth z = function
  | 0 -> z.head
  | n -> nth (z.tail()) (n - 1)

let rec constant x = {head = x ; tail = fun () -> constant x }

(* Some examples *)

let sqrt2 =
  {head = 1 ; tail = fun () -> constant 2} (* 1,2,2,2,2... *)

let golden_ratio = constant 1   (* 1,1,1,1,1,1 *)

let rec take n z =
  if n = 1 then [z.head]
  else z.head::(take (n-1) (z.tail()))


(* Q3.1: implement the function q as explained in the pdf *)
let rec q z n =
  if n = 0 then 1
  else if n = 1 then nth z 1
  else (nth z n) * q z (n-1) + q z (n-2)
  

(* Q3.2: implement the function r as in the notes *)
let rec r z n = 
  if n = 0 then float_of_int (z.head)
  else let denom = ( float_of_int ( (q z (n-1) )*(q z n) ) ) in
    if denom = 0.0 then r z (n-1) (*avoid division by zero*)
    else (r z (n-1) +. (-1.0)**(float_of_int (n-1))/. denom )

(* Q3.3: implement the error function *)
let error z n = 
  let qn = (q z n) in
  let denom = float_of_int(qn * (qn + q z (n-1))) in
  if denom = 0.0 then denom (*avoid division by zero*)
  else 1.0 /. denom

(* Q3.4: implement a function that computes a rational approximation of a real number *)
let rat_of_real z approx = 
  let rec helper str n = 
    if error str n < approx then r str n
    else helper str (n+1)
  in helper z 1 

let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r = 
  let hd = int_of_float (floor r) in
  let decimal = r -. float_of_int hd in
  {head = hd; 
  tail = (fun () -> if (epsilon_float > decimal || decimal = 0.0) then constant 0
                    else real_of_rat (1.0 /. decimal) )
  }
(*#use "hw4.ml";;*)


(* Examples *)

(* Approximations of the  irrational numbers we have *)

(* let sqrt_2_rat = rat_of_real sqrt2 1.e-5 *)
(* let golden_ratio_rat = rat_of_real golden_ratio 1.e-5 *)

(* To test the representation of rationals we can try this *)
(* let to_real_and_back n = rat_of_real (real_of_rat n) 0.0001 *)

(* lye1 should be very close to 10 (it is exact 10 in the model solution) *)
(* let e1 = to_real_and_back 10.0 *)

(* this is the float approximation of pi, not the real number pi *)
(* let not_pi = 2. *. acos 0. *)

(* This should share the same 4 decimals with not_pi *)
(* let not_pi' = to_real_and_back not_pi *)
