-- | 

module Haskell.RealWorldHaskell.Ch3DefineTypeStreamFunctions.Note where

import Data.List
type BookID = Int
type BookName = String
type BookAuthor = [String]

data BookInfo = Book BookID BookName BookAuthor
              deriving Show

type CustomerID = Int

type Address = String

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

john = Customer  12 "John" "where place"

primes =filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /=0 ]
a=[1,2,3]
b=a
c= [x | x<-a]

foo = x
    where x = y
          
            where y = 2


data List a = Cons a (List a)
            | Nil
              deriving (Show)

getFromList :: List a -> [a]
getFromList (Cons a la) = a : getFromList la
getFromList Nil = []

lac = Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))

getListLength :: [a] -> Int
getListLength (a:la) = 1 + getListLength la
getListLength [] = 0

getMeanOfList :: Fractional a => [a] -> a
getMeanOfList [] = 0
getMeanOfList ln = sum ln / fromIntegral (length ln)


-- just same with reverse
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (a : la) = reverseList la ++ [a]

getPalindrome :: [a] -> [a]
getPalindrome [] = []
getPalindrome la = la ++ reverseList la


isPalindrome :: Eq a => [a] -> Bool
isPalindrome la = reverseList la == la

sortListOfList :: [[a]] -> [[a]]
sortListOfList lla = sortBy (\a b -> compare (length a) (length b) ) lla

myIntersperse :: a -> [[a]] -> [a]
-- myIntersperse ins [[]] = []   -- not match, why?
myIntersperse ins [] = []
myIntersperse ins (la : lla) = la ++ [ins] ++ myIntersperse ins lla


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

lengthTree :: Tree a -> Int
lengthTree Empty = 0
lengthTree (Node a ta tb) = 1 + max (lengthTree ta) (lengthTree tb)

treec = Node "x" Empty (Node "y" Empty Empty)

data AngelDirection = Left | Right | OneLine

data Point = Point {xPos :: Float , yPos :: Float}

-- what is left? the angle could be to up or down!
-- decideAngleDirection :: Point -> Point -> Point -> AngelDirection
-- decideAngelDirection a b c = 

main :: IO()
main = do
  let a = [1,2,3]
  let b = a
  let b = map (+1) a
  print b
  print a
  print c
  print $ getFromList lac
  
