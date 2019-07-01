-- | 

module Haskell.RealWorldHaskell.Ch3FunctionalProgramming.Note where
import Data.Char (digitToInt)
import Data.Foldable
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'                

data a `Pair` b = a `Pair` b
                  deriving (Show)


safeHead :: [a] -> Maybe a
safeHead la =
  case la of
    [] -> Nothing
    _ -> Just $ head la

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail la = Just $ tail la

safeLast :: [a] -> Maybe a
safeLast la =
  case la of
    [] -> Nothing
    _  -> Just $ last la

safeInit :: [a] -> Maybe [a]
safeInit la =
  if null la then Nothing
  else Just $ init la

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f la = takeWhile f la :[dropWhile f la]


-- con is a fold too
mapWithFoldr :: (a -> b) -> [a] -> [b]
mapWithFoldr f la = foldr ((:) . f) [] la 

asIntWithFoldl :: String -> Maybe Int
-- order reversed ,not right
-- asIntWithFold str = foldr ( (\s b -> 10 * b + s) . digitToInt) 0 str
asIntWithFoldl str
  | elem '.' str = Nothing
  | length str  < 20 =
    if head str =='-' then Just $ foldl' ((\b s -> 10 * b - digitToInt s)) 0 (tail str)
    else Just $  foldl' ((\b s -> 10 * b + digitToInt s)) 0 str
  | otherwise =  Nothing

concatWithFoldr :: [[a]] -> [a]
concatWithFoldr lla = foldr (++) [] lla

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (a : as)
  | f a = a : myTakeWhile f as
  | otherwise = []

takeWhileWithFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileWithFoldr f la = foldr decide [] la
  where decide a b | f a = a : b
                   | otherwise = []

-- describe what you should do in detail, and replace "what" in possible granularity
-- with function on hand in that granularity
groupByFoldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldl f la = foldl' cof [] la
  where cof  b a
          | null b = [[a]]
          | f ((head . last) b) a = init b ++  [last b ++ [a]] --true, a join last sublist of b
          | otherwise = b ++ [[a]] -- false, add a as the last sublist of b
-- actually get more neat way shown in wordsWithFoldr, just figure out : profounder


anyWithFoldl :: Foldable t => (a -> Bool) -> t a -> Bool
anyWithFoldl f la = foldl' (\b a -> b || f a) False la

cycleWithFoldr la = foldr f [] la
  where f a b = [a] ++ b ++ foldr f [] la

-- never show anything
-- cycleWithFoldl la = foldl' f [] la
  -- where f b a = foldl' f [] la ++   b ++ [a]

next :: Int -> [a] -> a
next num (x : xs) =
  if num > 0 then next (num-1) xs else x


wordsWithFoldr :: String -> [String]
wordsWithFoldr str = (\(c,s) -> if null c then s else c:s) $ foldr f ([],[]) str
  where f a (newSub,b)
          -- | null b = [[a][]]
          | isSpace a =
            if null newSub
            then (newSub , b)
            else ([], newSub : b)
          | otherwise = (a : newSub,  b )
          where isSpace a = a == '\n' || a == '\r' || a == ' '


unlinesWithFoldr :: [String] -> String
unlinesWithFoldr str =
  foldr (\a b -> a ++ "\n" ++ b) [] str

