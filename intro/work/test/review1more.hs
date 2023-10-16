{-
  CISC 360, Fall 2023

  Week 4, part 2

  review1more.hs: A couple more examples
-}

module Review1more where

{- The Formula type will probably appear on the actual quiz.
-}
data Formula
  = Or Formula Formula
  | Not Formula
  | Atom String
  deriving (Show)

{-
Write a Haskell expression of type Formula that corresponds to the following parse tree:

        Not
         |
        Or
       /  \
   Atom    Atom
    |       |
   ""      ".."

[To check that your solution is accepted by Haskell, replace 'undefined' with your solution.]

Method:
 1. Write the root constructor.
 2. For each child, write (    ).
 3. Recurse.
-}
formula :: Formula
formula = Not (Or (Atom "") (Atom ".."))

{- Try this yourself first, then scroll down -}

{-
     Not (Or (Atom "") (Atom ".."))
[obtained as follows:
     Not (  )
     Not (Or (  ) (  ))
     Not (Or (Atom "") (  ))
     Not (Or (Atom "") (Atom ".."))

]
-}

{-
  Example of computing the *longest atom string* in a formula:

  For all formulas p,
    longest p  ==  n

  where n is the length of the longest atom string.

  For example,  longest (Not (Atom "abcde"))  ==  5
                  because the longest (the only!) atom string is "abcde".

                longest (Or (Atom "abc") (Not (Atom "")))  ==  3
                  because the longest atom string is "abc" (length 3),
                  compared to "" (length 0) and 3 > 0.

  I'll write one clause per constructor of Formula, so one clause for Or, one for Not, and one for Atom.

  I'll use built-in functions 'length', which returns the length of a string (actually, of any list),
                             and 'max', which returns the greater of two integers.
-}
longest :: Formula -> Int -- why Int and not Integer?  length returns Int, not Integer
longest (Or p q) = max (longest p) (longest q)
longest (Not p) = longest p
longest (Atom s) = length s

{-
  Examples of stepping  longest:

    longest (Not (Atom "abcdefghijklmnopqrstuvwxyz"))
 => longest p
 =  longest (Atom "abcdefghijklmnopqrstuvwxyz")    by fun. app. with (Atom "abcdefghijklmnopqrstuvwxyz") for p
 => length s
    with "abcdefghijklmnopqrstuvwxyz" for s
 =  length "abcdefghijklmnopqrstuvwxyz"            by fun. app.
 => 26                                             by length

    longest (Or (Not (Atom "abcdefghijklmnopqrstuvwxyz")) (Atom "xyz"))
 => max (longest p) (longest q)                    by fun. app.
    with (Not (Atom "abcdefghijklmnopqrstuvwxyz")) for p,
         (Atom "xyz") for q
 => max (longest (Not (Atom "abcdefghijklmnopqrstuvwxyz"))) (longest (Atom "xyz"))
 ...  (see previous example)
 => max 26 (longest (Atom "xyz"))
 => max 26 (length "xyz")                   by fun. app. with "xyz" for s
 => max 26 3                                by length
 => 26                                      by max
-}
