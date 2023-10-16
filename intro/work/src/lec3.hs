-- CISC 360, Fall 2023
-- Jana Dunfield
-- lec3.hs
-- Code for week 1, part 3 (360-lec3.pdf)

triple z = 3 * z

triple2 = \z -> 3 * z

-- triple and triple2 are different ways of writing the same thing

fl = 4.1

i = 3

t = i + fl

{-
   Haskell has a variety of types for numbers, which work roughly
   like this:

      - Numbers like 3 and 20 are compatible with any numeric type
        (any type in the "type class" Num).

      - Numbers like 3.14 and 0.002 are compatible with any fractional
        type (any type in the type class Fractional).

      - Some specific numeric types:

           Double   floating-point (in Fractional and in Num)
           Int      integer, usually 64-bit (in Num)
           Integer  integer, "arbitrary precision" or "bignum":
                      essentially unlimited number of digits

      - Sometimes, Haskell will choose a specific type that "works":

        i = 3      -- where i isn't mentioned elsewhere:
                      Haskell will choose Integer

        fl = 4.1   -- where fl isn't mentioned elsewhere:
                      Haskell will choose Double

        i = 3         -- because i is added to 4.1, Haskell chooses
        t = i + 4.1   --  the type of i to be Double
                      --  (you can't add an Integer to a Double)

   Confusingly, Haskell behaves differently when using the interactive
    "toplevel" (read-eval-print loop):
    if you type

      let i = 3

    it will not choose the type of i to be Integer, but will leave it
    unspecified as some type in the type class Num.
-}
diag :: Int -> Int
diag n = if n == 0 then 0 else n + diag (n - 1)

{-
      add3 4
   => ((+) 3) 4
   =   (+) 3  4

   Writing ( ) around an operator like + or == changes it from
   an infix operator

       3 + 4

   to a prefix operator

       (+) 3 4

   Try some of the following in GHCi:

    :type (+)
    (+)
    (+) 3 4
    (==) 3 4
    :type (==)
    :type (==) 3
    :type (==) (3 :: Integer)
    (==) 3
    ((==) 3) 5
-}

{-
Are  num and integer the same?

Integer is a type
Num is a type class; Integer is a Num
Int and Float are also Nums

Int is a fixed-size integer type (probably about 63 bits)

Integer is arbitrary-precision integers (bignums)

["type", "class", "category", "sort", "variety"
can be synonyms in general English but their meanings in
technical contexts, such as programming, can differ]

\*Main> :type (==) 3
(==) 3  ::  (Eq a, Num a) => a -> Bool

(Eq a, Num a) => a -> Bool
\^^^^^^^^^^^^^^^^
type guard
condition on the type a
Eq a   the type a has to support equality
Num a  the type a has to be numeric (for example, Integer)

(Eq a, Num a) => a -> Bool
                 ^^^^^^^^^
                 function that takes  a  as input
                 and returns  Bool as the output

-}

myfun = \w -> not w

something = \f -> f (f False)

f1 :: Integer -> Integer -> Integer
f1 = (\x -> (\y -> x + y))

func (x, y) = y - x

lambdanand = (\p -> \q -> not (p && q))

lambdanandTrue = lambdanand True

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

guardsheffer2 p q
  | (p == True) && (q == True) = False
  | (p == True) && (q == False) = True
  | (p == False) && (q == True) = True
  | (p == False) && (q == False) = True

guardsheffer3 p q
  | (p == True) && (q == True) = False
  | True = True

guardsheffer4 p q
  | (p == True) && (q == True) = False
  | False = True