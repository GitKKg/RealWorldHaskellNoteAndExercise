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

asIntWithFold :: String -> Maybe Int
-- order reversed ,not right
-- asIntWithFold str = foldr ( (\s b -> 10 * b + s) . digitToInt) 0 str
asIntWithFold str
  | elem '.' str = Nothing
  | length str  < 20 =
    if head str =='-' then Just $ foldl' ((\b s -> 10 * b - digitToInt s)) 0 (tail str)
    else Just $  foldl' ((\b s -> 10 * b + digitToInt s)) 0 str
  | otherwise =  Nothing

