{-
  CISC 360, Fall 2023

  Week 4, part 2

  review1.hs: Review lecture focused on Quiz 1.
-}

module Review1 where

{- The Formula type will probably appear on the actual quiz.
-}
data Formula
  = Or Formula Formula
  | Not Formula
  | Atom String
  deriving (Show)

{-
General advice for CISC 360:
   Make files  q1.hs, q2.hs, q3.hs, q4.hs, q5.hs, q6.hs

Write a Haskell expression of type Formula that corresponds to the following parse tree:

       Or
      /   \
   Atom   Atom
     |     |
    "t"   "u"

[To check that your solution is accepted by Haskell, replace 'undefined' with your solution.]

Method:
 1. Write the root constructor.
 2. For each child, write (    ).
 3. Recurse.
-}
formula1 :: Formula
formula1 = Or (Atom "u") (Atom "u")

{-
  Extra ( ) are okay **if Haskell accepts them**.
-}

{-
  For each of the following expressions:
    If the expression cannot be stepped, briefly explain why it cannot be stepped.

    If the expression can be stepped, step the expression as far as possible.
    With each step, give the justification ("by arithmetic", etc.).
    For steps by function application, give the substitution ("4 for x", etc.).

 R1.  (\f -> f + f) 2
   =>  f + f with 2 for f             by fun. app. with 2 for f
   =   2 + 2
   =>  4                     by arithmetic

 R2.  (\x -> 2 + 2)
    No argument given, so it doesn't step.
  or:  Does not step because we can't step inside a lambda.

 R3.  (\f -> (\x -> x)) (\y -> y)
   => (\x -> x)      by function application with (\y -> y) for f

-}

-- ("involution" is a function or operator that is its own inverse, like
--   logical negation)
-- ¬¬(a > 0)  ≡  (a > 0)

involute :: Formula -> Formula
involute (Or q1 q2) = Or (involute q1) (involute q2)
involute (Not (Not q)) = (involute q)
involute (Not q) = Not (involute q)
involute p = p

myFormula :: Formula
myFormula = Or (Not (Atom "i")) (Not (Not (Atom "i")))

mFormula :: Formula
mFormula = involute myFormula

{-
  Step the expression below, as far as possible.
  Justify each step ("by function application", etc.).
  In function application steps, write the substitution, e.g.: Atom "z" for w

   involute (Not (Not (Not (Atom "m"))))
[
   does (Not (Not (Not (Atom "m")))) match (Or q1 q2)
   Is it possible for (Not (Not (Not (Atom "m")))) = (Or q1 q2)?   No
   does (Not (Not (Not (Atom "m")))) match (Not (Not q)) ?
   Is it possible for Not (Not (Not (Atom "m")))
                    = Not (Not q               ) ?  Yes, and
                       q = (Not (Atom "m"))
]
=> (involute q)          by pattern matching with (Not (Atom "m")) for q
=  (involute (Not (Atom "m")))
[   is it possible for (Not (Atom "m"))
                     = (Not (Not q))  ?     No
    is it possible for (Not (Atom "m"))
                     = (Not q)        ?     Yes, with (Atom "m") for q
=> Not (involute (Atom "m"))      by fun. app. with (Atom "m") for q
=> Not p                          by fun. app. with (Atom "m") for p
=  Not (Atom "m")
-}
