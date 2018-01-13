
type prop = Atom of string
          | Not of prop
          | And of prop * prop
          | Or of prop * prop

let impl (p, q) = Or(Not p, q)
let iff (p, q) = And (impl (p,q), impl (q, p))

let mp = impl (And (Atom "P", impl (Atom "P", Atom "Q")), Atom "Q")

(* Proof by contradiction (reduction ad absurdum): ((¬ P) ⇒ Q) ∧ ((¬ P) ⇒ (¬ Q)) ⇒ P *)
let ra = impl (
             And (impl (Not (Atom "P"),
                        Atom "Q"),
                  impl (Not (Atom "P"),
                        Not (Atom "Q"))),
             Atom "P")

(* Atoms and their negations *)
type signed_atom
  = PosAtom of string
  | NegAtom of string

(* In NNF negations can only be applied to atoms, this datatype only
   allows for propositions in NNF *)
type nnf
  = AndN of nnf * nnf
  | OrN of nnf * nnf
  | AtomN of signed_atom

(* Q1.2: Write the function nnf that converts propositions into NNF,
   notice that the typechecker will guide you in your answer as the
   type forces the result to be in NNF. Your job is to not forget any
   sub-parts of the proposition in the conversion. *)
let rec to_nnf : prop -> nnf = function
  | Atom a -> AtomN (PosAtom a)
  | Not (Atom a) -> AtomN (NegAtom a)
  | Not (Not p) -> to_nnf p
  | Not (And (p, q)) -> OrN (to_nnf (Not p), to_nnf (Not q))
  | Not (Or (p, q)) -> AndN (to_nnf (Not p), to_nnf (Not q))
  | And (p, q) -> AndN (to_nnf p, to_nnf q)
  | Or (p, q) -> OrN (to_nnf p, to_nnf q)

let mp_nnf = to_nnf mp

let ra_nnf = to_nnf ra

(* Q1.3: Write a datatype cnf that represents only propositions in
   cnf. Hint: You might want to use more than one type to be able to
   represent sub-expressions.*)

(* a non_empty list of atoms *)
type dis = Extra of signed_atom * dis
         | Single of signed_atom

type cnf =
  | AndC of dis * cnf
  | OrC of dis

(* Q1.4: Write the distribute and nnf_to_cnf functions using the new
   datatype. Hint: you may need more than one helper function. *)
let rec distribute : cnf * cnf -> cnf = function
  | p, AndC (q, r) -> assoc_conj(distribute (p, OrC q), distribute (p, r))
  | AndC(q, r), p ->  assoc_conj(distribute (OrC q, p), distribute (r, p))
  | OrC p, OrC q -> OrC (assoc_disj (p, q))

and assoc_disj : dis * dis -> dis = function
  | Extra(a, p), Extra(b, q) -> assoc_disj (Extra(a, Extra(b,p)), q)
  | Single p, q -> Extra (p, q)
  | q, Single p -> Extra (p, q)

and assoc_conj : cnf * cnf -> cnf = function
  | OrC p, q -> AndC (p, q)
  | AndC(q, ps), ps' -> AndC (q, assoc_conj (ps, ps'))

let rec nnf_to_cnf : nnf -> cnf = function
  | AndN(p, q) -> let p' =  nnf_to_cnf p in
                  let q' = nnf_to_cnf q in
                  assoc_conj (p', q')
  | OrN(p, q) -> distribute (nnf_to_cnf p, nnf_to_cnf q)
  | AtomN a -> OrC (Single a)

let to_cnf (p :prop) : cnf = nnf_to_cnf (to_nnf p)

(* Q1.5: Write the new positives and negative atoms function as in the
   previous version *)
let rec positives = function
  | (Single (PosAtom a)) -> [a]
  | (Single (NegAtom _)) -> []
  | (Extra (PosAtom a, q)) -> a :: positives q
  | (Extra (NegAtom a, q)) -> positives q

let rec negatives = function
  | (Single (PosAtom _)) -> []
  | (Single (NegAtom a)) -> [a]
  | (Extra (PosAtom a, q)) -> negatives q
  | (Extra (NegAtom a, q)) -> a::negatives q

(* Fill in the code for the intersection function from Q1.1 *)
let rec intersection l1 l2 = match l1 with
  | [] -> []
  | x::xs -> if List.mem x l2 then x::intersection xs l2 else intersection xs l2

(* Q1.6: Write the new cnf_tautology function *)
let rec cnf_tautology : cnf -> bool = function
  | AndC (p, q) -> dis_tautology p && cnf_tautology q
  | OrC p -> not ([] = intersection (positives p) (negatives p))

and dis_tautology p = not ([] = intersection (positives p) (negatives p))

let taut (p : prop) : bool = cnf_tautology (to_cnf p)
let unsat (p : prop) : bool = taut (Not p)
let sat (p : prop) : bool = not (unsat p)
