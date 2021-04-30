-- Josh Levy

-- Used for question #7 to convert char to int and check if character is a digit
import Data.Char (digitToInt, isDigit)

-- #1
-- dotproduct takes a two vectors (lists of numbers) and computes the dot product of the vectors. If one list is longer than the other, you can ignore the extra numbers of the longer list.
dotproduct :: Num p => [p] -> [p] -> p
dotproduct l1 [] = 0
dotproduct l1 l2 = (head l1 * head l2) + dotproduct (tail l1) (tail l2)

-- --------------------------------------------------------------
-- #2
-- vectormult takes a row vector (a list of numbers) and matrix (a list of lists of numbers) and multiplies the vector times the matrix. The result is a vector where the ith element of the result is the dotproduct of the input vector and the ith column of the matrix. You can assume that the length of the vector matches the number of rows of the matrix.

-- helper function to map a function onto a list
mymap :: (a1 -> a2) -> [a1] -> [a2]
mymap func [] = []
mymap func lis = func (head lis) : mymap func (tail lis)

vectormult :: Num a => [a] -> [[a]] -> [a]
vectormult vec ([] : t) = []
vectormult vec mat = dotproduct vec (mymap head mat) : vectormult vec (mymap tail mat)

-- --------------------------------------------------------------
-- #3
-- matrixmultiply takes two matrices (a list of lists of numbers) and multiplies them. You can assume the number of columns of the first matrix is equal to the number of rows of the second matrix.
-- in the same sublist
matrixmultiply :: Num a => [[a]] -> [[a]] -> [[a]]
matrixmultiply [] m2 = []
matrixmultiply m1 m2 = vectormult (head m1) m2 : matrixmultiply (tail m1) m2

-- --------------------------------------------------------------
-- #4
-- bubbledown takes a BinaryTree as input. If the element stored in the root is larger than either children, swap the element with the smaller child, and recurse on the child you swapped the element with. The recursion should stop when either you reach a leaf or when the element of the node is smaller than both its children.
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Show)

-- Returns the value of an InnerNode
getValue :: BinaryTree p -> p
getValue (Leaf a) = a
getValue (InnerNode a l r) = a

-- replaces the root of an InnerNode with the inputted value
replaceRoot :: BinaryTree t -> t -> BinaryTree t
replaceRoot (Leaf a) e = Leaf e
replaceRoot (InnerNode a l r) e = InnerNode e l r

bubbledown :: Ord t => BinaryTree t -> BinaryTree t
bubbledown Empty = Empty
bubbledown (Leaf a) = Leaf a
bubbledown (InnerNode a l r)
  | a > getValue l && getValue l < getValue r = InnerNode (getValue l) (bubbledown (replaceRoot l a)) r
  | a > getValue r && getValue r < getValue l = InnerNode (getValue r) l (bubbledown (replaceRoot r a))
  | otherwise = InnerNode a l r

-- --------------------------------------------------------------
-- #5
-- Create a type that allows us to have nested lists. Your type should have two kinds of values, elements and sublists.
data NestedList t = Element t | SubList [NestedList t] deriving (Show)

-- --------------------------------------------------------------
-- #6
-- Create the function flatten that takes a list as above and returns a list with just the elements.
flatten :: [NestedList t] -> [NestedList t]
flatten [] = []
flatten [Element t] = [Element t]
flatten [SubList t] = flatten t
flatten (h : t) = flatten [h] ++ flatten t

-- --------------------------------------------------------------
-- #7
-- Create the function string2list that takes a string containing single digits and parentheses (possibly spaces or commas) and create a list.

-- Helper function to return the remainder of the string after a closing parenthesis
getRestOfString :: (Eq a, Num a) => [Char] -> a -> [Char]
getRestOfString (')' : t) 0 = t
getRestOfString ('(' : t) c = getRestOfString t (c + 1)
getRestOfString (')' : t) c = getRestOfString t (c - 1)
getRestOfString (h : t) c = getRestOfString t c

-- return the string up to the correct closing parenthesis
getCloseParenthesisString = []

string2list :: [Char] -> NestedList t
string2list (h : t) 
  | h == ')' = string2list t
  | h == '(' = SubList [string2list (getCloseParenthesisString t)] : string2list t
  | h == ' ' || h == ',' = string2list t
  | isDigit h = Element (digitToInt h) : string2list t
  |otherwise =  error "Incorrect Character"

-- --------------------------------------------------------------
-- #8
-- The function takes a value of some type, a list of the same type (as a monad), and a test function and returns a list (in a monad). If the first passes the test (the test function on that element returns true), the element is appended to the monad list. Otherwise the result is Nothing.
yourfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
yourfunction a Nothing func = Nothing
yourfunction a (Just s) func
  | func a = Just (a : s)
  | otherwise = Nothing

-- Using your above function, create a function checklist that takes a list and a function and returns Nothing if the elements in the list fail to past the function and the list (embedded in a Maybe) if all the elements pass.
checklist :: [a] -> (a -> Bool) -> Maybe [a]
checklist [] func = Just []
checklist (h : t) func = yourfunction h (checklist t func) func

-- --------------------------------------------------------------
-- #9
-- Create the function inorder that takes a list that contains lists of numbers and returns a Maybe containing the list if all the numbers in the sublists are in non-decreasing order from left to right, and it should return Nothing if the numbers are not in non-decreasing order.

-- helper function that returns the end of the list if l1 is sorted, or the head of the l2 plus one if it is not sorted. This will cause the inorderHelper to return Nothing if it is false since the head of l2 + 1 will be greater than the head of h2. This function allows for a linear linear traversal.
isInorder :: (Ord a, Num a) => [a] -> [a] -> [a]
isInorder [l1] l2 = [l1]
isInorder (a : (b : t)) l2
  | a <= b = isInorder (b : t) l2
  | otherwise = [head l2 + 1]

-- helper function that returns Just if the entire list is inorder or Nothing if it is not (returns a Maybe). The function takes two parameters so that Just can return the entire list
inorderHelper :: (Ord a, Num a) => [[a]] -> t -> Maybe t
inorderHelper [] l = Just l
inorderHelper [a] l = Just l
inorderHelper (h : (h2 : t2)) l
  | null h || null h2 = inorderHelper (h2 : t2) l
  | head (isInorder h h2) <= head h2 = inorderHelper (h2 : t2) l
  | otherwise = Nothing

inorder :: (Ord a, Num a) => [[a]] -> Maybe [[a]]
inorder l = inorderHelper l l

-- --------------------------------------------------------------
-- #10
-- Create a list monad that generalizes a list. This will not be a Haskell Monad type, but instead one of our own creation like the Value type from lecture. Then create a binding function lbind and a return function lreturn to make a list monad.
data Pair t = Pair t (Pair t) | Null deriving (Show)

-- Takes an element and returns a Pair with the element
lreturn :: t -> Pair t
lreturn t = Pair t Null

-- Takes two Pairs and appends them together
lappend :: Pair t -> Pair t -> Pair t
lappend Null t = t
lappend (Pair a b) p = Pair a (lappend b p)

-- Applies (binds) a function onto a Pair
lbind :: Pair t1 -> (t1 -> Pair t2) -> Pair t2
lbind Null t = Null
lbind (Pair a b) func = lappend (func a) (lbind b func)