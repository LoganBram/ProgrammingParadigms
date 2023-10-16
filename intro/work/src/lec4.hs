-- CISC 360, Fall 2023
-- Jana Dunfield
--
-- Code for Week 2 part 1 (360-lec4.pdf)

-- doublesecond_v1, doublesecond_v2, doublesecond all do the same thing

doublesecond_v1 x y = y * 2

doublesecond_v2 x = (\y -> y * 2)

doublesecond = (\x -> (\y -> y * 2))

{-
  Today:

   • Stepping

    doublesecond_v2 3 10
equivalent to
    (doublesecond_v2 3) 10
 =>
    ...
 => 20

    (\x -> (\y -> y * 2)) 3 10
    ...
-}

mystery = doublesecond_v2 99

{-
   Haskell allows (but, usually, does not require) _type declarations_ (type signatures).
   The symbol  ::  is read "has type".

   "triple has type   Integer   ->        Integer"
                      ^^        ^^        ^^^^^^^
                argument type   function  result type
   See 360-lec4.pdf, section 1, for discussion.
-}
triple :: Integer -> Integer
triple z = 3 * z

-- from lec3.hs
diag :: Integer -> Integer
diag n = if n == 0 then 0 else n + diag (n - 1)

{-
   NAND (not-AND) truth table:

    p      q      nand p q
    ------ ------ -----------
    True   True   False
    True   False  True
    False  True   True
    False  False  True
-}
nand :: Bool -> Bool -> Bool
-- with an incorrect type declaration
-- nand :: Bool -> Bool
-- we get an error message that essentially says
--              Bool
--              is not the same as
--              Bool -> Bool
nand p q = not (p && q)

nandthree :: Bool -> (Bool -> (Bool -> Bool))
nandthree p q r = not (p && q && r)

nandthreealt = (\p -> (\q -> (\r -> (not (p && q && r)))))

{-
   Writing
     nand p q = not (p && q)
   is really the same as using two lambdas (\):

   nand = (\p -> \q -> not (p && q))
-}
lambdanand = (\p -> \q -> not (p && q))

{-
      ((\p -> \q -> not (p && q)) False) True
   => (\p -> \q -> not (False && q)) True
   => (\p -> \q -> not (False && q))        -- incorrect stepping

      ((\p -> \q -> not (p && q)) False) True   step the whole expr by stepping
                                                ((\p -> \q -> not (p && q)) False) True
   => (\q -> not (False && q)) True          by function application
   => (not (False && q)) with True subst. for q      by function application
    = (not (False && q))[True/q]             φ[t/x]  (CISC 204)
    = (not (False && True))                  formula φ with t subst. for x
   => not False
   => True

      lambdanand False
   => (\p -> \q -> not (p && q)) False
   => (\q -> not (p && q))[False/p]
   =  (\q -> not (False && q))            :: Bool -> Bool

   (Try  :type nand  and  :type lambdanand)
-}

{-
  The body of lambdanand consists of a lambda

            (\p -> ...               )

  The body of that lambda is

                   \q -> ...

  and the body of *that* lambda is

                         not (p && q)

  Moving back out and thinking about the types, we have

            (\p -> \q -> not (p && q))
                         ^^^^^^^^^^^^ Bool
                   ^^^^^^^^^^^^^^^^^^ Bool -> Bool
                                      ^^^^
                                      type of q
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
              Bool -> Bool -> Bool

            "Bool -> Bool -> Bool" says:
  I accept   Bool
  and         return
                     Bool -> Bool.

  So if we call lambdanand with True
  (equivalently: "apply lambdanand to True")
  we get something of type
                     Bool -> Bool.

            "Bool -> Bool -> Bool" says:
I accept 1st Bool,
                 2nd Bool,
                      return Bool
-}

lambdanandTrue = lambdanand True

-- Try  :type lambdanandTrue

lambdanandTrueTrue = lambdanandTrue True

-- Try  :type lambdanandTrueTrue
lambdanandTrueFalse = lambdanandTrue False

-- nand and lambdanand behave in exactly the same way,
-- even though they are written differently

nandTrue = nand True -- nand with p True

nandFalse = nand False -- nand with p False

{-
  So, even though you can think of nand as a function
  of two arguments (it takes *two* Bools),
  you can also apply it to only one argument,
  which returns a function.
  This is called "partial application" or "currying".
  It makes it easy to specialize a function on its
  first argument.
-}

-- (+), (-), etc. are prefix versions of +, -, etc.
-- They can be partially applied:

add100 :: Integer -> Integer
add100 = (+) 100

one100 = add100 1

twelve100 = add100 12

-- sub1 :: Integer -> Integer
-- sub1 = (-) 1
--        (-) 1 5    1 - 5   -4
-- same as
-- sub1 = (\x -> 1 - x)

-- sub1 x  =  x - 1
sub1 :: Integer -> Integer
sub1 = (\x -> x - 1)

sub2 :: Integer -> Integer
sub2 = (\x -> (-) x 1)

-- or could say
--     \x -> (-) x 1

-- Aside:
-- Specializing a function on its *second* argument is
-- a little harder, but possible:
--
nand_q_True = (\p -> nand p True)

nand_p_False = (\q -> nand False q)

--           nand False

-- A longer way to write nand.
-- Another name for nand is "Sheffer stroke",
-- so I will call this definition "sheffer".

sheffer p q =
  if (p == True) && (q == True)
    then False
    else
      if (p == True) && (q == False)
        then True
        else
          if (p == False) && (q == True)
            then True
            else True

-- Alternate alternate way of writing nand,
-- using _guards_.

-- vertical bar | similar to set notation  "Let S = {x | x > 3}"
--                                                     ^ "such that"
guardsheffer p q
  | (p == True) && (q == True) = False
  | (p == True) && (q == False) = True
  | (p == False) && (q == True) = True
  | (p == False) && (q == False) = True

-- Alternate alternate alternate way of writing nand,
-- using _guards_ with a "default" or "fall-through".
guardsheffer2 p q
  | (p == True) && (q == True) = False
  | otherwise = True

guardsheffer3 p q
  | (p == True) && (q == True) = False
  | (p == True) && (q == False) = True
  | (p == False) && (q == True) = True

--  | (p == False) && (q == False) = True

--
-- can't say this (yet)
-- guardsheffer4 :: Bool -> Bool -> (Bool or Integer)
-- guardsheffer4 p q
--  | (p == True) && (q == True)   = False
--  | (p == True) && (q == False)  = 3      -- type error
--  | (p == False) && (q == True)  = True
--  | (p == False) && (q == False) = True

{-  q :: Bool -> Bool
                ->
               /  \
              /    \
            ->     Bool
           /  \
          /    \
       Bool   Bool

    (\q -> q False) (\y -> y)
           ^^^^^^^  ^^^^^^^^^
           body     arg
       substitute arg for q in body
=>   substitute (\y -> y) for q in body
 =   (\y -> y) False
            ^  ^^^^^
         body   arg
=>   y with False substituted for y
 =   False

(In CISC 204, I wrote substitutions like this:

phi[t/x]

"substitute t for x in the formula phi"
or equivalently
"replace x with t in the formula phi"
-}