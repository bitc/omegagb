-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Prerequisites
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module should be imported from all other modules
--
----------------------------------------------------------------------------- 
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

