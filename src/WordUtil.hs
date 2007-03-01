-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  WordUtil
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- Utility Functions for dealing with 8 bit and 16 bit values.
--
-----------------------------------------------------------------------------

module WordUtil where

import Data.Char
import Data.Word
import Data.Bits
import qualified Numeric as N

joinWord16 :: Word8 -> Word8 -> Word16
joinWord16 hi lo =
  ((fromIntegral lo)::Word16) + (shiftL ((fromIntegral hi)::Word16) 8)

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 nn =
  let hi = fromIntegral (shiftR nn 8)
      lo = fromIntegral nn in
  (hi, lo)

showHex n = '$' : (N.showHex n "")

showHex1 :: Word8 -> String
showHex1 n =
  let s = map toUpper (N.showHex n "") in
  if n > 0xF then s else '0' : s

showHex2 :: Word16 -> String
showHex2 n =
  let s = map toUpper (N.showHex n "") in
  if n > 0xF
  then (if n > 0xFF
        then (if n > 0xFFF
              then s
              else '0' : s)
        else "00" ++ s)
  else "000" ++ s

