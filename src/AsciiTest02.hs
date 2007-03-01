-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  AsciiTest02
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module executes a ROM and does some ascii art output. To use this,
-- run it in a terminal with a really tiny font.
--
-----------------------------------------------------------------------------

module AsciiTest02 where
import Prerequisites

import Data.Char
import Data.Array.IArray
import Data.Array.MArray
import Control.Monad

import RomImage
import MachineIO
import MachineStateIO
import Joypad

romFile = "roms/Dropzone (U) (GB).gb"

test02 :: IO ()
test02 = do
  romImage <- loadRomImage romFile

  machineState <- initMachineStateIO romImage

  let clear = putStr ((chr 27) : "[H")

  let pixel c = case c of
                0 -> ' '
                1 -> '~'
                2 -> '='
                3 -> '@'

  let printRow :: ScanLine -> IO ()
      printRow scanline = mapM_ ( \x -> readArray scanline x >>= ( \c -> putChar (pixel c) ) ) [0..159]
                          >> putChar '\n'

--let s = concatMap ( \x -> (pixel (d!(y, x))) ) [0..159] in putStrLn s

  let printRows :: Array Int ScanLine -> IO ()
      printRows d = clear >> mapM_ ( \y -> printRow (d!y) ) [0..138]

  replicateM_ 5000 ((updateMachineDisplayFrameIO machineState) >> printRows (msDisplay machineState))

