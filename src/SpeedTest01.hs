-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  AsciiTest01
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module executes a part of a ROM.
--
-----------------------------------------------------------------------------

module SpeedTest01 where
import Prerequisites

import Data.Char
import Data.Array.IArray --hiding ((!), (//))
import Control.DeepSeq

import RomImage
import Machine
import Joypad

romFile = "roms/space.gb"

-- Force evaluation of elements of the list by reading the (0,0) memory location
iter 1 f x = let (r, x') = f x in
  r
iter n f x = let (r, x') = f x in
  r `seq` iter (n-1) f x'

evaluate r = r!(0,0) `seq` return ()

test01 :: IO ()
test01 = do
  romImage <- loadRomImage romFile
  let initialState = initialMachineState romImage


  let v = iter 20 (updateMachineDisplayFrame (initJoypadKeyStates False False False False False False False False)) initialState
  evaluate v

