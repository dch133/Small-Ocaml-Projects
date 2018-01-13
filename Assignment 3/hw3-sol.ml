(* Question 1 - Unfolding *)

(* This is the function unfold, take some time to compare it it fold.
   If fold_left and fold_right take lists to generate a value, this
   function takes a value that will generate a list. The relation
   between folds and unfolds is the beginning of a wonderful tale in
   computer science. But let's not get ahead of ourselves.

   Unfold takes a function that from a seed value it generates a new
   element of the list, and a the seed for the next element, another
   function to stop the generation, and an initial seed.
*)

let rec unfold (f: 'seed -> ('a * 'seed)) (stop : 'b -> bool) (b : 'seed) : 'a list =
  if stop b then []
  else let x, b' = f b in
       x :: (unfold f stop b')

(* Q1.1: Return the even numbers up-to max *)
let evens max = unfold (fun b -> b, b + 2) (fun b -> max < b) 0

(* Q1.2: Return the Fibonacci sequence up-to max *)
let fib max = unfold (fun (n1, n2) -> n1, (n2, n1+n2)) (fun (x, _) -> x > max) (1, 1)

(* Q1.3: Return the list of rows of the Pascal triangle that are shorter than max *)
let pascal max = unfold
                   (fun r ->
                     let next = List.map2 (+) ([0] @ r) (r @ [0]) in
                     r, next)
                   (fun l -> List.length l >= max)
                   [1]

let rec zip (l1 : 'a list) (l2 : 'b list) :  ('a * 'b) list =
match l1, l2 with
| [], _ -> []
| _, [] -> []
| x::xs, y::ys -> (x, y) :: zip xs ys

(* Optional: implement zip with a single call to unfold *)
let zip' l1 l2 =
  unfold (fun (x::xs, y::ys) -> ((x,y), (xs, ys)))
         (fun (l1, l2) -> l1 = [] || l2 = []) (l1, l2)


(* Question 2 *)
let ugly x =
  let rec ackermann m n = match (m , n) with
    | 0 , n -> n+1
    | m , 0 -> ackermann (m-1) 1
    | m , n -> ackermann (m-1) (ackermann m (n-1))
  in
  ackermann 3 x

let memo_zero (f : 'a -> 'b) : 'a -> 'b = f

(* Q2.1: Write a function that memoizes the last value called. *)
let memo_one (f : 'a -> 'b) : ('a -> 'b) =
  let cache : ('a * 'b) option ref = ref None in
  let store x =
    let b = f x in cache := Some (x, b) ; b
  in
  (fun a -> match !cache with
            | None -> store a
            | Some (a', b') -> if a = a' then b' else store a)

let ugly' = memo_one ugly

let u1 = ugly' 3                (* this one calls ugly with 3 *)
let u2 = ugly' 3                (* this one uses the stored value *)
let u3 = ugly' 1                (* the stored value is for 3 so it calls ugly *)
let u4 = ugly' 2                (* the stored value is for 1 so it calls ugly *)
let u5 = ugly' 10               (* the stored value is for 2 so it calls ugly and takes a couple of seconds *)
let u6 = ugly' 10               (* the one uses the stored value and returns immediately *)

(* Q2.2: Write a function that memoizes the last value called. *)
let memo_many (n : int) (f : 'a -> 'b) : 'a -> 'b =
  if n < 1 then f else
  let cache : ('a * 'b) option array = Array.make n None in
  let cursor = ref 0 in
  let find a =
    let rec f n =
      match cache.(n) with
      | Some(a', b') when a = a' -> Some b'
      | _ -> if n > 0 then f (n - 1) else None
    in
    f (Array.length cache - 1)
  in
  let next () =
    incr cursor ; if !cursor >= Array.length cache then cursor := 0 else ()
  in
  let add a =
    let b = f a in
    cache.(!cursor) <- Some (a, b) ; next () ; b
  in
  fun a -> match find a with
           | None -> add a
           | Some b -> b

(* Question 3: Doubly-linked circular lists  *)

(* Some possibly useful functions *)

(* A function that returns the Greatest Common Denominator of two numbers *)
let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

(* A function that returns the least common multiple of two numbers *)
let lcm (m : int) (n : int) : int  =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

(* Circular doubly linked lists *)

(* The type of a cell (a non-empty circular list) *)
type 'a cell = { mutable p : 'a cell; data : 'a ; mutable n : 'a cell}

(* The type of possibly empty circular lists *)
type 'a circlist = 'a cell option

(* An empty circular list *)
let empty :'a circlist = None

(* A singleton list that contains a single element *)
let singl (x : 'a) : 'a circlist =
  let rec pointer = {p = pointer ; data = x ; n = pointer} in
  Some pointer

(* Rotate a list to next element *)
let next : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.n)

(* Rotate a list to previous element *)
let prev : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.p)

(* Q3.1: Write a function that add a new element at the beginning of a list *)
let cons (x : 'a)  (xs : 'a circlist) : 'a circlist =
  match xs with
  | None -> singl x
  | Some curr ->
     let prev = curr.p in
     let cell = {p = curr.p; data = x; n = curr} in
     curr.p <- cell ; prev.n <- cell ; Some curr

(* Q3.2: Write a function that computes the length of a list (Careful with the infinite loops)  *)
let rec length : 'a circlist -> int = function
  | None -> 0
  | Some head ->
     let rec walk cl =
       if cl.n == head then 1 else 1 + (walk cl.n)
     in
     walk head

(* Q3.3: Write a function that produces an immutable list from a circular list *)
let to_list : 'a circlist -> 'a list = function
  | None -> []
  | Some head ->
     let rec walk cl =
       if cl.n == head then [cl.data] else cl.data::(walk cl.n)
     in
     walk head

(* Once you've written cons you can use this function to quickly populate your lists *)
let rec from_list : 'a list -> 'a circlist = function
  | [] -> empty
  | x::xs -> cons x (from_list xs)

(* Q3.4 Write a function that reverses all the directions of the list *)
let rev (l : 'a circlist) : 'a circlist =
  let len = length l in
  let rec r l fuel =
    if fuel = 0 then ()
    else
      let tmp = l.n in
      begin
        l.n <- l.p ;
        l.p <- tmp ; r l.n (fuel -1)
      end
  in
  match l with
  | None -> None
  | Some e -> r e (len) ; (Some e.n)

(* OPTIONAL: Write the map function as applied to lists *)
let map (f : 'a -> 'b) : 'a circlist -> ' b circlist = function
  | None -> None
  | Some e ->
     let len = length (Some e) in
     let rec m h fuel =
       if fuel = 0 then empty
       else cons (f h.data) (m (h.n) (fuel -1))
     in rev(m e len)

(* OPTIONAL A function that compares two lists ignoring the rotation *)
let eq (l1 : 'a circlist) (l2 : 'a circlist) : bool =
  match l1, l2 with
  | None, None -> true
  | None, _
  | _, None -> false
  | Some e1, Some e2 ->
     let len1, len2 = length l1, length l2 in
     let steps = lcm len1 len2 in
     let rec cmp e1 e2 fuel =
       if fuel = 0 then true else
         if e1.data = e2.data then
           cmp (e1.n) (e2.n) (fuel -1)
         else
           false
     in
     let rec rot e1 e2 fuel =
       if fuel = 0 then false else
       if cmp e1 e2 steps then true else
       rot (e1.n) e2 (fuel - 1)
     in
     rot e1 e2 len1

let ex = cons 12 (cons 43 (cons 34 (singl 3)))
let lex = to_list ex

let l1 = from_list [true; true ; false]
let l2 = from_list [true; true ; false ; true; true ; false ; true] (* l1 and l2 are the tricky test *)
let l3 = from_list [true; true ; false ; true; true ; false]


let l4 = from_list ['a'; 'b'; 'a'; 'b']
let l5 = from_list ['a'; 'b'; 'a'; 'b'; 'a'; 'b']

let l6 = from_list ['a'; 'a']
let l7 = from_list ['a'; 'a'; 'a']

let l8 = from_list [1 ; 2 ; 3]
let l9 = from_list [3 ; 1 ; 2]  (* eq l8 l9 = true *)

let l10 = from_list [1 ; 2 ; 3]
let l11 = from_list [3 ; 2 ; 1]  (* eq l10 l11 = false *)
