-- Copyright 2006 mutantlemon.com

module Display where

import Data.Array.Unboxed
import Data.Word

type Display = UArray (Int, Int) Word8

blankDisplay :: Display
blankDisplay = array ((0, 0), (143, 159)) (map (\i -> (i, 0)) (range ((0, 0), (143, 159))))

