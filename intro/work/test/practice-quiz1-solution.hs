{-
  CISC 360, Fall 2023
  
  Sample solution to practice quiz 1
-}





module PracticeQuiz1 where

{- The Formula type will probably appear on the actual quiz.
-}
data Formula = Or Formula Formula
             | Not Formula
             | Atom String
             deriving Show

{- PQ1.
Write a Haskell expression of type Formula that corresponds to the following parse tree:

            Not
             |
             |
            Or
           /   \
          /     \
       Or       Not
      /   \       |
    Atom  Atom   Atom
     |     |      |
    "z"   "zz"  "#why"

[To check that your solution is accepted by Haskell, replace 'undefined' with your solution.]
-}
formula :: Formula
formula = Not (Or (Or (Atom "z") (Atom "zz")) (Not (Atom "#why")))

{-
[Notes:
  The question asks for an expression,
   Not (Or  ... )

  The declaration

    formula :: Formula
    formula =

  is *not* required.

  You may include a *correct* declaration of 'formula',
  but an incorrect declaration may result in marks being deducted.
]
-}


{- PQ2.
Write an English sentence that corresponds to this Haskell type declaration.

Your answer should explain every part of the type.
For example, if the question asked about the type  Integer -> (Bool, Integer),
your answer should clearly indicate the type the function takes as an argument
and the type returned (including that the first component of the result is boolean
and the second component is an integer).

  mystery :: Bool -> (Char, Integer) -> Bool
-}
{-
Sample answer 1:

  mystery is a function that takes two arguments.

  The first argument is a boolean.

  The second argument is a pair whose first component is a character and whose second component is an integer.
 
  mystery returns a boolean.

Sample answer 2:

  mystery is a function that takes a boolean, and returns a function that takes a pair whose first component is a character and whose second component is an integer; that function returns a boolean.
-}




{- PQ3.

  For the purposes of this question, a "bug" is something that does at least one of the following:

     • prevents the file from compiling (loading)
     • causes an error (when using the function in the intended way)
     • causes the function to loop forever, or to return the wrong result (when using the function in the intended way).

  Unwise coding practice (for example, if I named the second pattern variable in the Or clause "left2" instead of "right") isn't a bug unless it causes one of the problems listed above.  At best, mentioning non-bugs requires effort that would be better spent looking for actual bugs.

  The function  count_nots is supposed to count the number of times "Not" appears in a Formula.
 
  For example,  count_nots (Atom "a")  should return 0,  and
                count_nots (Not (Or (Not (Atom "zzz")) (Atom "0")))  should return 2.

  In this question, you are asked to find the bugs in some Haskell code.
  Explain what the bugs are, and fix them by writing a correct version of the code.
  Giving correct code by itself is not enough; you need to explain what the bugs are.
-}

{-
count_nots :: Formula -> Integer
count_nots (Or left right)  = count_nots left + count_nots right
count_nots (Not _)          = 1
count_nots (Atom "a")       = 0
-}

{-
  Write your explanation here:

  Bug 1: The clause for Not always returns 1,
         which ignores Nots inside the child of Not.
  Fix: Change the wildcard pattern _ to a pattern variable q
       so we can refer to it, and add the result of a recursive call on q.

  Bug 2: In the last clause, the pattern matches only when the string in the Atom is "a".
  Fix: change "a" to _, which matches any string.
-}
-- corrected version:
count_nots :: Formula -> Integer
count_nots (Or left right) = count_nots left + count_nots right
count_nots (Not q)          = 1 + count_nots q
count_nots (Atom _)         = 0

{- Notes:
   The above answer would get full marks, because:

     - It mentions all the bugs known to me.
     - It includes a corrected version.
     - For each bug, it explains what the problem is, why it is a problem, and how to fix it.

   Here is one example of an explanation that would not get full marks:

   "Bug 1: In the clause for Not, the pattern should be  Not q, and the body is missing  + count_nots q".
   That is just a description of the change to the program text.
   I can already get that information by comparing your
   corrected version to the original.
-}


{- PQ4.
  For each of the following expressions:
    If the expression cannot be stepped, briefly explain why it cannot be stepped.
    
    If the expression can be stepped, step the expression as far as possible.
    With each step, give the justification ("by arithmetic", etc.).
    For steps by function application, give the substitution ("4 for x", etc.).

 1.  (\x -> 6)

Cannot be stepped because the function (\x -> 6) is not applied to an argument.


 2.  (\x -> (\y -> y + 1)) 5

   (\x -> (\y -> y + 1)) 5
=> (\y -> y + 1)             by function application
                             with 5 for x
  [That's as far as the expression can be stepped.
   OPTIONAL to write "and cannot step further".]

 3.  (\f -> f 0) (\x -> x)

   (\f -> f 0) (\x -> x)
=> (\x -> x) 0           by function application
                         with (\x -> x) for f
=> 0                     by function application
                         with 0 for x
-}


{- PQ5
-}

leftmost :: Formula -> String
leftmost (Atom v) = v
leftmost (Or q _) = leftmost q
leftmost (Not q)  = leftmost q

{-
  Step the expression below, as far as possible.
  Justify each step ("by function application", etc.).
  In function application steps, write the substitution (e.g. "False for b").

   leftmost (Or (Atom "meow") (Atom "r"))
=> leftmost (Atom "meow")   by fun. app.
                            with (Atom "meow") for q
=> "meow"                   by fun. app.
                            with "meow" for v
-}

