module Prerequisites where

import Debug.Trace
import Data.Array.IArray

{-

(!) :: (Show i, IArray a e, Ix i) =>
                     a i e -> i -> e
a ! i = let (lowerBound, upperBound) = bounds a in
        if i >= lowerBound && i <= upperBound
        then a Data.Array.IArray.! i
        else error ("Read out of bounds (" ++ show lowerBound ++ ", " ++ show upperBound ++ "), index = " ++ show i)

(//) :: (Show i, Show e, IArray a e, Ix i) =>
                     a i e -> [(i, e)] -> a i e
a // l = let (lowerBound, upperBound) = bounds a in
         if all ( \(i, _) -> i >= lowerBound && i <= upperBound ) l
         then a Data.Array.IArray.// l
         else error ("Write out of bounds (" ++ show lowerBound ++ ", " ++ show upperBound ++ "), " ++ show l)

-}

