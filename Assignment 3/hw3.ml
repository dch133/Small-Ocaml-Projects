
(*David Gilbert and Daniel Chernis *)
(*260746680         260707258*)




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

let nats max = unfold (fun b -> b, b + 1) (fun x -> x > max) 0

(* Q1.1: Return the even numbers up-to max *)
let evens (max : int): int list = unfold (fun b -> b, b + 2) (fun b -> b > max) 0

(* Q1.2: Return the Fibonacci sequence up-to max *)
let fib max = 1:: unfold (fun (a,b) ->
    if a = 0 then (1, (1,1) )
    else (a+b, (b,a+b)) ) (fun (a,b) -> (a+b) > max) (0,1)

(* Q1.3: Return the list of rows of the Pascal triangle that are shorter than max *)
  
  let pascal max =
    if max  = 0 || max  = 1 then [[]] else
    let rec addLists l1 l2 = match (l1, l2) with
      |([],_) -> l2  
      |(_,[]) -> l1  
      |(x::xs, y::ys) -> (x+y)::(addLists xs ys)
    in
      let rec getRow n =
        let rec helper n acc =
        if n = 0 then acc
        else helper (n-1) (addLists (0::acc) acc)
        in
        helper n [1] 
      in
        [1] :: unfold (fun b -> (getRow b), (b+1) )  (fun b -> b >= max-1) 1


(*zip function*)        
let rec zip (l1 : 'a list) (l2 : 'b list) :  ('a * 'b) list =
match l1, l2 with
| [], _ -> []
| _, [] -> []
| x::xs, y::ys -> (x, y):: zip xs ys

(* (Extra credit) Optional: implement zip with a single call to unfold *)
let zip' l1 l2 = unfold (
  function 
  |(x,[]) -> (0,0), ([],[])
  |([],y) -> (0,0), ([],[])
  |(x::xs, y::ys) -> (x,y), (xs,ys) ) (fun (xs,ys) -> xs = [] || ys = [] ) (l1,l2)

(* Question 2 *)

let ugly x =
  let rec ackermann m n = match (m , n) with
    | 0 , n -> n+1
    | m , 0 -> ackermann (m-1) 1
    | m , n -> ackermann (m-1) (ackermann m (n-1))
  in
  ackermann 3 x

let memo_zero (f : 'a -> 'b) : 'a -> 'b = f

(* Q2.1: Write a function that memorizes the last value called. *)

let memo_one f =
  let cache =  ref None in 
    fun newInpt -> match !cache with
      |Some (inpt,res) -> if newInpt = inpt then res
                     else (cache := Some (newInpt, f newInpt); f newInpt )
      |None -> cache := Some (newInpt, f newInpt); f newInpt

(* Example usage:

let ugly' = memo_one ugly

let u1 = ugly' 3                (* this one calls ugly with 3 *)
let u2 = ugly' 3                (* this one uses the stored value *)
let u3 = ugly' 1                (* the stored value is for 3 so it calls ugly *)
let u4 = ugly' 2                (* the stored value is for 1 so it calls ugly *)
let u5 = ugly' 10               (* the stored value is for 2 so it calls ugly and takes a couple of seconds *)
let u6 = ugly' 10               (* the one uses the stored value and returns immediately *)

 *)

(* Q2.2: Write a function that memoizes the last value called. *)
let memo_many n f =
  let cache =  ref None in
      let r = ref 0 in
  fun newInpt -> match !cache with
      |Some l -> if List.exists (fun (inpt,res) -> if inpt = newInpt then (r := res; true) else false) l
                 then !r
                 else 
                     if (List.length l = n) then let l = List.tl l in
                     ( cache := Some ( l @ [(newInpt, f newInpt)] ) ; f newInpt )
                     else ( cache := Some ( l @ [(newInpt, f newInpt)] ) ; f newInpt )

      |None -> cache := Some [(newInpt, f newInpt)] ; f newInpt
      


(* Question 3: Doubly-linked circular lists  *)

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
let cons (x : 'a)  (xs : 'a circlist) : 'a circlist = match xs with
  |None -> singl x
  |Some h -> let cell = {p = h.p ; data = x ; n = h} in 
              (h.p).n <- cell; h.p <- cell;                                         
              Some h


(* Q3.2: Write a function that computes the length of a list (Careful with the infinite loops)  *)
let rec length (l : 'a circlist) : int = match l with
    |None -> 0
    |Some head -> let rec counter h acc = if head==h then acc+1 else counter h.n (acc+1)
                  in counter head.n 0

     (* TEST CASES : let c1 = cons 4 (cons 5 empty) ;; next c1 ;; let c2 = cons 7 c1 ;; length c2 ;;  *)

(* Q3.3: Write a function that produces an immutable list from a circular list *)
let to_list (l : 'a circlist)  : 'a list = match l with 
      | None -> []
      | Some head -> let rec helper circ size l1 =        
                        if (size > 0) then helper circ.n (size-1) (circ.data :: l1)
                        else l1    
                     in helper head (length l) []

(* Once you've written cons you can use this function to quickly populate your lists *)
let rec from_list : 'a list -> 'a circlist = function
  | [] -> empty
  | x::xs -> cons x (from_list xs)

(* Q3.4: Write a function that reverses all the directions of the list *)
let rev (l : 'a circlist) : 'a circlist = match l with 
      |None -> l
      |Some head -> let rec order cell headCell =       
                        if cell == headCell
                        (*When done reversing list, rotate it one last time*)
                        then   
                        let temp = cell.p in
                        cell.p <- cell.n; cell.n <- temp; (*rev head*)
                        else begin 
                          let temp = cell.p in
                          cell.p <- cell.n; cell.n <- temp; 
                          order cell.p headCell end (*with pointes inversed .p is going right*)
                    in order head.n head;
                    Some head.n.n (*Shift the circlist so it points the the same elem as before rev*)

(* (Extra credit) OPTIONAL: Write the map function as applied to lists *)
let map (f : 'a -> 'b) : 'a circlist -> ' b circlist = 
  (fun l -> (*generate a list*)
    let size = length l in 
      let rec iterate f l l' size  = match l with (*l' is the new list to output*)
        | None -> empty
        | Some x ->  if size = 0 then l'
                     else iterate f (next (Some x)) (cons (f x.data) l') (size-1) 
      in iterate f l empty size
  ) 

(* Some possibly useful functions (Wink, wink!) *)
(* A function that returns the Greatest Common Denominator of two numbers *)
let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

(* A function that returns the least common multiple of two numbers *)
let lcm (m : int) (n : int) : int  =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)


(* (Extra credit) OPTIONAL A function that compares two lists ignoring the rotation *)
let eq (l1 : 'a circlist) (l2 : 'a circlist) : bool = match l1,l2 with
    |None,None -> true
    |None,_ -> false
    |_,None -> false
    |Some x,Some y ->       (*size = bigger list*)
            let initSize = if length l1 > length l2 then length l1
                           else length l2 in
                        
                           let rec compare l1 l2 size (b : bool) = 
                             if size = 0 then b
                             else if l1.data = l2.data then 
                                compare l1.n l2.n (size-1) true
                        (*some pair of values don't match in the middle of list = false*)  
                                  else if size != initSize then false 
                        (*this checks that at the beginning of the compare,
                        the pointers will align with the same values if possible*)
                                 else compare l1 l2.n size false
                        in compare x y initSize false 

(* Some examples *)
(*
let ex = cons 12 (cons 43 (cons 34 (singl 3)))
let lex = to_list ex

let l1 = from_list [true; true ; false]
let l3 = from_list [true; true ; false ; true; true ; false]

let l4 = from_list ['a'; 'b'; 'a'; 'b']
let l5 = from_list ['a'; 'b'; 'a'; 'b'; 'a'; 'b']

let l6 = from_list ['a'; 'a']
let l7 = from_list ['a'; 'a'; 'a']

let l8 = from_list [1 ; 2 ; 3]
let l9 = from_list [3 ; 1 ; 2]  (* eq l8 l9 = true *)

let l10 = from_list [1 ; 2 ; 3]
let l11 = from_list [3 ; 2 ; 1]  (* eq l10 l11 = false *)
*)
