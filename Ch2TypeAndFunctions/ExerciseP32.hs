-- | 

module Haskell.RealWorldHaskell.Ch2TypeAndFunctions.ExerciseP32 where

lastButOne :: [a] -> a
-- every left or previous operand should be thought as a shell of right or right rest expression
lastButOne list = head . (($) <$> flip drop <*> (\x -> x-2) . length) $ list 
