data Building
  = Goodwin
  | WalterLight
  | Beamish
  | Dupuis
  deriving (Show)

-- building is a data type, value of type are walterlight beamish etc
-- each thing after = is a constructor, Goodwing is type building, walterlight is type build etc
-- deriving Show prints oonstrcutor names

has_el :: Building -> Bool
has_el Goodwin = True
has_el WalterLight = True
has_el Beamish = True
has_el Dupuis = False

-- has_el WalterLight, is there a way to make WalterLight equal Walterlight? yes, run code.

my_and :: Bool -> Bool -> Bool
my_and True True = True
my_and True False = False
my_and False True = False
my_and False False = False

-- input my_and False True, matches third returns False

--------------TREES----------------

data Tree
  = Empty
  | Branch Tree Integer Tree
  deriving (Show)

-- a Tree is either empty or branch l k r, where l is a tree, k is an integer and r is a tree
-- Branch is a contructor not a tree, it is a function that takes a tree, an integer and a tree and returns a tree

mysum :: [Integer] -> Integer
mysum [] = 0
mysum (x : xs) = x + (mysum xs)

-------Functions as arguments------
