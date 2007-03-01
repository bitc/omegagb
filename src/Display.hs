-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Display
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module defines the Display type that represents the Game Boy's LCD
-- display. The Game Boy LCD display has a resolution of 144x160 and 4
-- colors,
--
-----------------------------------------------------------------------------

module Display where

import Data.Array.Unboxed
import Data.Word

type Display = UArray (Int, Int) Word8

blankDisplay :: Display
blankDisplay = array ((0, 0), (143, 159)) (map (\i -> (i, 0)) (range ((0, 0), (143, 159))))

