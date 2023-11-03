{-
  CISC 360, Fall 2023
  Jana Dunfield

  Questions possibly relevant to Quiz 2
-}

data Codeblock
  = End
  | Up Codeblock
  | Down Codeblock
  | Spin Codeblock Integer
  deriving (Show)

{-
Q1.
Write a Haskell expression of type Codeblock that corresponds to the following tree:

      Up
       |
     Spin
    /    \
 Down    360
  |
 End
-}
expr1 :: Codeblock
expr1 = Up (Spin (Down End) 360)

{-
Q2.
The type declaration

  me :: t -> t

says that me is a function that takes something of type t, and returns something of type t.

Briefly explain, in English, what the following type declaration says.

  mystery :: Maybe (b -> Codeblock) -> (b, Codeblock) -> [Codeblock]

(Don't speculate about what such a function would actually do.)
-}

-- first argument is either Nothing or Just f, where f is a function that takes b and returns a codeblok
-- second argument is a tuple where first comp is type b and second is codeblock
-- returns list of type codeblock

{-
Q3.
Write a function g such that

  rotate g (Spin End 2)        returns   Spin (Up End) 2
and
  rotate g (Up (Down End))     returns   Down End

  Begin by writing the appropriate type declaration for g.

  Hint: Use pattern matching to define g.
-}
rotate :: (Codeblock -> Codeblock) -> Codeblock -> Codeblock
rotate z (Up block) = Down (rotate z block)
rotate z (Spin block n) = Spin (rotate z block) n
rotate z block = z block

g :: Codeblock -> Codeblock
g End = Up End
g (Down End) = End

data Shrub a
  = Blue a
  | Red
  | One (Shrub a)
  | Two (Shrub a) (Shrub a)
  deriving (Show, Eq)

{-
Q4.
  In this question, you are asked to find the bugs in some Haskell code.  Explain what the bugs are, and fix them by writing a correct version of the code.  Giving correct code by itself is not enough; you need to explain why the bugs are bugs.

  Here are two strange functions, splat and blat.

  'splat' takes a Painting, changes two adjacent Rs to one G, and removes Bs.
  For example:

   splat (B (R (R (G Canvas))))  returns  G (G Canvas):
          ^  ^^^^  ^- preserved           ^  ^
          |   ||                          |  from G
          |  changed to G                 |
         removed                      from R R

  The function  blat  is supposed to behave like 'splat' when its first argument
  is (\x -> x).

  For example,

   blat (\x -> x) (B (R (R (G Canvas))))  returns G (G Canvas).

  More precisely, for all paintings p, we want:

   blat (\x -> x) p  ==  splat p

  However, there are two bugs in the definition of 'blat'.
  Find the bugs, and:

  1. Explain what they are and why they are bugs;

  2. Fix the bugs and give a correct definition of 'blat'.
-}
data Painting
  = Canvas
  | R Painting
  | G Painting
  | B Painting
  deriving (Show, Eq)

splat :: Painting -> Painting
splat (R (R p)) = G (splat p)
splat (G p) = G (splat p)
splat (B p) = splat p
splat p = p

blat :: (Painting -> b) -> Painting -> b
blat k (R (R p)) = blat (\q -> k (G q)) p
blat k (G p) = blat (\q -> k q) p
blat k (B p) = blat k p
blat k p = k p

{-
Q5. This question asks you to step a Haskell expression.
-}
data Tree
  = Leaf
  | Branch Tree Tree
  deriving (Show, Eq)

follow :: Tree -> [Integer]
follow Leaf = [0]
follow (Branch (Branch _ _) t2) = 5 : follow t2
follow (Branch t1 t2) = 1 : follow t1

-- Using the above function definition, step the expression below as far as possible.
-- For each step, state the justification ("by arithmetic", "by function application", etc.).
-- When stepping by function application, state the substitution (e.g. "Leaf for t2").
{-
   follow (Branch (Branch Leaf Leaf) (Branch Leaf Leaf))
=>

-}
