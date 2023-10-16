data Formula
  = Or Formula Formula
  | Not Formula
  | Atom String
  deriving (Show)

size_con :: Formula -> Integer
size_con (Or q1 q2) = 10 + size_con q1 + size_con q2 -- Corrected weight for Or
size_con (Not q) = 3 + size_con q -- Corrected weight for Not
size_con (Atom _) = 1 -- The weight for Atom is already correct

myFormula :: Formula
myFormula = Not (Or (Atom "b") (Atom ""))

mFormula :: Integer
mFormula = size_con myFormula
