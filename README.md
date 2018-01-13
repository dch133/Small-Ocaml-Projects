# Small-Ocaml-Projects

Assignment 1: Fimiliarising my self with ocaml syntax

Assignment 2: 
Q1.1: Defining an intersection function

Q1.2: Convert propositions to  Negation Normal Form (NNF)

Q1.3: Defining a type Conjunctive Normal Form (CNF)

Q1.4: Implement funtion distribution over CNF experssions and convert NNF to CNF

Q1.5: collect positives and negatives from a proposition in CNF

Q1.6: implement the function cnf_tautology: cnf -> bool that tells us when a proposition in CNF is a tautology

Q2: Use structural induction to prove that a head-recurive expression and a tail-recursiove version are equivalent

Assignment 3:
Q1.1: Return a list of successive even numbers starting with 0

Q1.2: Write a function let fib (max : int): int list = ... that returns a list of Fibonacci numbers that are smaller than max

Q1.3: Write a function let pascal (max : int): int list list =. .. that returns
      the list of rows of the Pascal triangle that are shorter than max
      
Q1.4: Consider the function zip that takes two lists and returns a tuple of elements from each list
      Write a short implementation of this function using unfold-function
      
Q2.1: Write the function memo_one. It stores one value in 'cache'

Q2.2: Write the function memo_many. It stores n-values in 'cache' , given n-value

Q3.1: Write the function 'cons' that adds an element to a circular list

Q3.2: Write the function 'length' that gives the length of a circular list

Q3.3: Write the function 'to_list' that converts a circular list to a list

Q3.4: Write the function 'rev' that that reverses the directions of the circular list

Q3.5: Write the function 'map' that maps a function to a circular list in an analogous way to List.map.

Q3.6: Write the function 'eq' that compares two lists. In particular we want the following properties to
      hold of equality:
      
        1. Given a list l it is always the case that eq l (next l)= true
        
        2. Given a list l it is always the case that eq l (prev l)= true
        
        3. Lists with different lengths may be equal if their contents go together one by one.

Assignment 4:
Q1.1: Implement a function to backtrack throught a tree with Exceptions

Q1.2: Implement a function to backtrack throught a tree with Fail Continuations

Q1.3:  Implement a function to backtrack throught a tree with Success Continuations

Q2.1: Implement a module named: FractionArith of type Arith that uses a value of type fraction as its internal representation

Q2.2: Implement a functor named: Newton (using the Newton-Raphson method) that takes a module 
      of type Arith to produce a module of type NewtonSolver.
      This module should allow us to approximate the square root function two internal representations (floats and fractions)
      
Q3.1: Implement the function q : int stream -> int -> int

Q3.2: Implement the function r: int stream -> int -> float

Q3.3: Implement the function error: int stream -> int -> float Using the definition above.
      This function takes the stream, the number n of terms in the approximation
      and returns a bound for the error
      
Q3.4: Implement the function val rat_of_real : int stream -> float -> float
      that takes a string and an error bound and computes a rational approximation of the
      real number using the functions r, q and error. Because we are getting an approximation,
      our function will always inspect finitely many elements of the stream.
      
Q3.5: Implement the function real_of_rat : float -> int stream that computes the real number representation of floating point value

Assignment 5:
Q1 - Q2: Extending semantics definitions on paper (see PDF)

Q3.1: Implement a module called DeadCode of type Optimization so we can eliminate expressions
      that are assigned to a variable but the variable is never used
      
Q3.2: Implement a module called RemoveLetMatch of type Optimization to transform pattern matching 'lets' into the
      regular 'lets' and then take the pairs apart using projections
