(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)

let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a =
  match t with
  | Node (e, ts) -> if p e then e else find_many p ts

and find_many p ts =
  match ts with
  | [] -> raise BackTrack
  | t::ts ->
     try find_e p t
     with BackTrack -> find_many p ts

(* Q1.1: write this function and it helper functions *)
let find p t = try Some (find_e p t) with BackTrack -> None

(* Find with failure continuations *)

let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option =
  match t with
  | Node (e, ts) -> if p e then Some e else find_many_k p ts k

and find_many_k p ts k = match ts with
  | [] -> k ()
  | t::ts -> find_k p t (fun () -> find_many_k p ts k)

(* Q1.2: write this function and it helper functions *)
let find' p t = find_k p t (fun () -> None)

(* Find all with continuations *)

let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b =
  match t with
  | Node (e, ts) ->
     if p e then find_many_all p ts (fun es -> k (e::es))
     else find_many_all p ts k

and find_many_all p ts k =
  match ts with
  | [] -> k []
  | t::ts -> find_all_k p t (fun es -> find_many_all p ts (fun es' -> k (es @ es')))

(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun x -> x)

(* An example to use *)

let example = Node (7, [ Node (1, [])
                         ; Node (2, [Node (16, [])])
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
  let epsilon = 1, 1000000
  let from_fraction (num, den) = (num, den)
  let plus (n1, d1) (n2, d2) = (n1 * d2 + n2 * d1, d1 * d2)
  let minus (n1, d1) (n2, d2) = plus (n1, d1) (-n2, d2)
  let prod (n1, d1) (n2, d2) = (n1 * n2, d1 * d2)
  let div (n1, d1) (n2, d2) = (n1 * d2, n2 * d1)
  let abs (n1, d1) = (abs n1, abs d1)
  let lt (n1, d1) (n2, d2) = (float_of_int n1 /. float_of_int d1) < (float_of_int n2 /. float_of_int d2)
  let le (n1, d1) (n2, d2) = (float_of_int n1 /. float_of_int d1) <= (float_of_int n2 /. float_of_int d2)
  let gt (n1, d1) (n2, d2) = (float_of_int n1 /. float_of_int d1) > (float_of_int n2 /. float_of_int d2)
  let ge (n1, d1) (n2, d2) = (float_of_int n1 /. float_of_int d1) >= (float_of_int n2 /. float_of_int d2)
  let eq (n1, d1) (n2, d2) = (float_of_int n1 /. float_of_int d1) = (float_of_int n2 /. float_of_int d2)
  let to_string (n1, d1) =
    let rec gcd (u : int) (v : int) : int =
      if v <> 0 then (gcd v (u mod v))
      else (Pervasives.abs u)
    in
    let n = gcd n1 d1 in
    let n',d' = n1/n, d1/n in
    (* let f = float_of_int n' /. float_of_int d' in *)
    string_of_int n' ^ " / " ^ string_of_int d' (* ^ " (" ^ string_of_float f ^ ")" *)
end

module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)
module Newton (A : Arith) : (NewtonSolver with type t = A.t) =
  struct
    type t = A.t

    let square_root a =
      let rec findroot x acc =
        let next = A.div (A.plus (A.div a x) x) (A.from_fraction (2, 1)) in
        if A.lt (A.abs (A.minus x  next)) acc
        then next
        else findroot next acc
      in
      findroot (A.from_fraction (1,1)) A.epsilon
  end

module FloatNewton = Newton (FloatArith)
module RationalNewton = Newton (FractionArith)

let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (2, 1))
let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (2, 1))

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
  assert (n >= 0) ;
  match n with
  | 0 -> 1
  | 1 -> nth z 1
  | n -> nth z n * q z (n-1) + q z (n-2)

(* Q3.2: implement the function r as in the notes giving an extra "fuel" parameter to see how many terms do you need *)
let rec r z fuel =
  let rec f n =
    assert (n >= 0);
    let num = if n mod 2 = 0 then -1 else 1 in
    let den = q z (n-1) * q z n in
    if den = 0 then 0.0
    else
    float_of_int num /. float_of_int den
  in
  match fuel with
  | 0 -> float_of_int z.head
  | n -> let x = f n in let y = r z (n - 1) in x +. y

(* Q3.3: implement the error function *)
let error z n =
  let qzn = q z n in
  let den = qzn * (qzn + q z (n -1)) in
  if den = 0 then 0.0
  else  1. /. (float_of_int den)

(* Q3.4: implement a function that computes a rational approximation of a real number *)
let rat_of_real z approx =
  let compute_fuel z approx =
    let rec compute_fuel fuel =
      if error z fuel < approx then fuel else compute_fuel (fuel + 1)
    in
    compute_fuel 1
  in
  let fuel = compute_fuel z approx in
  r z fuel

let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r =
  let n = floor r in
  let rest = r -. n in
  let tail = if rest <= epsilon_float then fun () -> constant 0
             else fun () -> real_of_rat (1. /. rest)
  in
  {head = int_of_float n ; tail = tail}


(* Examples *)

let sqrt_2_rat = rat_of_real sqrt2 1.e-5
let golden_ratio_rat = rat_of_real golden_ratio 1.e-5
