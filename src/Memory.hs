-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Memory
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module does emulation of the Game Boy's memory architecture.
-- Still lots more work to do.
--
-----------------------------------------------------------------------------

module Memory where
--import Prerequisites

import Data.Array.Unboxed
import Data.Word
import Data.Bits

import Debug.Trace

import RomImage
import Joypad
import WordUtil

class MemoryModel a where
  readMem :: a -> Word16 -> Word8
  writeMem :: a -> Word16 -> Word8 -> a

data Memory = Memory {
  memRomImage :: RomImage,
  memRam :: UArray Word16 Word8,
  memJoypadKeyStates :: JoypadKeyStates
}

initMemory :: RomImage -> Memory
initMemory ri = Memory {
  memRomImage = ri,
  memRam = (listArray (0x8000, 0xFFFF) (replicate 0x8000 0x00)) //
    [
(0xFF00, 0x30),
(0xFF05, 0x00),
(0xFF06, 0x00),
(0xFF07, 0x00),
(0xFF10, 0x80),
(0xFF11, 0xBF),
(0xFF12, 0xF3),
(0xFF14, 0xBF),
(0xFF16, 0x3F),
(0xFF17, 0x00),
(0xFF19, 0xBF),
(0xFF1A, 0x7F),
(0xFF1B, 0xFF),
(0xFF1C, 0x9F),
(0xFF1E, 0xBF),
(0xFF20, 0xFF),
(0xFF21, 0x00),
(0xFF22, 0x22),
(0xFF23, 0xBF),
(0xFF24, 0x77),
(0xFF25, 0xF3),
(0xFF26, 0xF1),
(0xFF40, 0x91),
(0xFF42, 0x00),
(0xFF43, 0x00),
(0xFF45, 0x00),
(0xFF47, 0xFC),
(0xFF48, 0xFF),
(0xFF49, 0xFF),
(0xFF4A, 0x00),
(0xFF4B, 0x00),
(0xFFFF, 0x00)
    ],
  memJoypadKeyStates = initJoypadKeyStates False False False False False False False False

  {- XXX -}  -- Rom Bank
}


instance MemoryModel Memory where
  readMem m a
    | a < 0x8000  = readRomImageByte (memRomImage m) (fromIntegral a)
    | a == 0xFF00 = let p0 = (memRam m)!0xFF00
                        boolBit :: Int -> Bool -> Word8
                        boolBit n b = if b then 0x00 else bit n in
                    if not (testBit p0 4)
                    then let p10 = boolBit 0 (getJoypadKeyStateRight (memJoypadKeyStates m))
                             p11 = boolBit 1 (getJoypadKeyStateLeft (memJoypadKeyStates m))
                             p12 = boolBit 2 (getJoypadKeyStateUp (memJoypadKeyStates m))
                             p13 = boolBit 3 (getJoypadKeyStateDown (memJoypadKeyStates m))
                         in p10 .|. p11 .|. p12 .|. p13
                    else if not (testBit p0 5)
                         then let p10 = boolBit 0 (getJoypadKeyStateA (memJoypadKeyStates m))
                                  p11 = boolBit 1 (getJoypadKeyStateB (memJoypadKeyStates m))
                                  p12 = boolBit 2 (getJoypadKeyStateSelect (memJoypadKeyStates m))
                                  p13 = boolBit 3 (getJoypadKeyStateStart (memJoypadKeyStates m))
                              in p10 .|. p11 .|. p12 .|. p13
                         else 0x0F
    | a == 0xFF01 = readRam
    | a == 0xFF02 = readRam
    | a == 0xFF03 = readRam
    | a == 0xFF04 = readRam -- DIV Register, should work
    | a == 0xFF05 = readRam
    | a == 0xFF06 = readRam
    | a == 0xFF07 = readRam
    | a == 0xFF08 = readRam
    | a == 0xFF09 = readRam
    | a == 0xFF0A = readRam
    | a == 0xFF0B = readRam
    | a == 0xFF0C = readRam
    | a == 0xFF0D = readRam
    | a == 0xFF0E = readRam
    | a == 0xFF0F = readRam -- IF, Interrupt Flag
    | a == 0xFF10 = readRam
    | a == 0xFF30 = readRam
    | a == 0xFF33 = readRam
    | a == 0xFF34 = readRam
    | a == 0xFF35 = readRam
    | a == 0xFF36 = readRam
    | a == 0xFF37 = readRam
    | a == 0xFF38 = readRam
    | a == 0xFF39 = readRam
    | a == 0xFF3A = readRam
    | a == 0xFF3B = readRam
    | a == 0xFF3C = readRam
    | a == 0xFF3D = readRam
    | a == 0xFF3E = readRam
    | a == 0xFF3F = readRam
    | a == 0xFF40 = readRam
    | a == 0xFF41 = readRam -- TODO STAT, LCDC Status TODO this is some bizarre read/write register
    | a == 0xFF42 = readRam -- SCY Scroll Y
    | a == 0xFF43 = readRam -- SCX Scroll X
    | a == 0xFF44 = readRam
    | a == 0xFF45 = readRam
    | a == 0xFF47 = readRam -- TODO BGP, BG & Window Pallette Data, do some shit here
    | a == 0xFF48 = readRam -- TODO OBP0, Object Pallette 0 Data, do some shit here
    | a == 0xFF49 = readRam -- TODO OBP1, Object Pallette 1 Data, do some shit here
--    | a >= 0xFF00 && a < 0xFF4C = error ((showHex a) ++ " IO Register NOT IMPLEMENTED")
    | otherwise   = readRam

    where readRam = (flip const) ("read " ++ showHex2 a) ((memRam m) ! a)
  
  writeMem m a v
    | a < 0x8000                = m
    | a == 0xFF00               = if v == 0x00 || v == 0x20 || v == 0x10 || v == 0x30 then writeRam else error ("$FF00 P1, Joypad, not allowed: " ++ show v) -- P1 Joypad
    | a == 0xFF01               = writeRam
    | a == 0xFF02               = writeRam
    | a == 0xFF03               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF04               = m { memRam = (memRam m)//[(a, 0x00)] } -- DIV (Divider Register)
    | a == 0xFF05               = writeRam -- TODO Timer counter TIMA probably don't need to do anything here
    | a == 0xFF06               = writeRam -- TODO Timer Modulo TMA maybe we need to do some shit here
    | a == 0xFF07               = if not (testBit v 2) then writeRam else error ("$FF07 TAC, Timer Control Register, not allowed: " ++ show v) -- TODO Timer Control TAC maybe we need to do some shit here
    | a == 0xFF08               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF09               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF0A               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF0B               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF0C               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF0D               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF0E               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF0F               = writeRam -- IF Interrupt Flag
    | a == 0xFF10               = writeRam
    | a == 0xFF11               = writeRam
    | a == 0xFF12               = writeRam
    | a == 0xFF13               = writeRam
    | a == 0xFF14               = writeRam
    | a == 0xFF15               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF16               = writeRam
    | a == 0xFF17               = writeRam
    | a == 0xFF18               = writeRam
    | a == 0xFF19               = writeRam
    | a == 0xFF1A               = writeRam
    | a == 0xFF1B               = writeRam
    | a == 0xFF1C               = writeRam
    | a == 0xFF1D               = writeRam
    | a == 0xFF1E               = writeRam
    | a == 0xFF1F               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF20               = writeRam
    | a == 0xFF21               = writeRam
    | a == 0xFF22               = writeRam
    | a == 0xFF23               = writeRam
    | a == 0xFF24               = writeRam
    | a == 0xFF25               = writeRam
    | a == 0xFF26               = writeRam
    | a == 0xFF27               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF28               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF29               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF2A               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF2B               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF2C               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF2D               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF2E               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF2F               = writeRam -- TODO wtf is this register??? allow the write for now
    | a == 0xFF30               = writeRam
    | a == 0xFF31               = writeRam
    | a == 0xFF32               = writeRam
    | a == 0xFF33               = writeRam
    | a == 0xFF34               = writeRam
    | a == 0xFF35               = writeRam
    | a == 0xFF36               = writeRam
    | a == 0xFF37               = writeRam
    | a == 0xFF38               = writeRam
    | a == 0xFF39               = writeRam
    | a == 0xFF3A               = writeRam
    | a == 0xFF3B               = writeRam
    | a == 0xFF3C               = writeRam
    | a == 0xFF3D               = writeRam
    | a == 0xFF3E               = writeRam
    | a == 0xFF3F               = writeRam
    | a == 0xFF40               = writeRam -- TODO LCDC, LCD Control, maybe do shit
    | a == 0xFF41               = writeRam -- if v .&. 0x78 == 0 then m else error ("$FF41 STAT Register, tried to turn on interrupt, not allowed " ++ show v ++ " LYC=" ++ show ((memRam m)!0xFF45))-- TODO STAT, LCDC Status, only modify bits 3,4,5,6! maybe do some other shit
    | a == 0xFF42               = writeRam -- TODO SCY, Scroll Y, maybe do shit
    | a == 0xFF43               = writeRam -- TODO SCX, Scroll X, maybe do shit
    | a == 0xFF44               = m { memRam = (memRam m)//[(a, 0x00)] } -- error "write to IO Register $FF44 (LY - LCDC Y Coordanite)"
    | a == 0xFF45               = writeRam -- TODO LYC, LY Compare, maybe do shit
    | a == 0xFF46               = m { memRam = (memRam m)//
                                               (map ( \i ->
                                                        (i,
                                                         (flip const)
                                                           ("DMA: " ++ showHex2 ((i-0xFE00)+(joinWord16 v 0)) ++ " " ++ showHex1 v)
                                                           (readMem m ((i-0xFE00)+(joinWord16 v 0))))
                                                    )
                                                    [0xFE00..0xFE9F])
                                    }
    | a == 0xFF47               = writeRam -- TODO BGP, BG & Window Pallette Data, maybe we need to do some shit here
    | a == 0xFF48               = writeRam -- TODO OBP0, Object Palette 0 Data, maybe we need to do some shit here
    | a == 0xFF49               = writeRam -- TODO OBP1, Object Palette 1 Data, maybe we need to do some shit here
    | a == 0xFF4A               = writeRam -- TODO WY, Window Y Position, maybe we need to do some shit here
    | a == 0xFF4B               = writeRam -- TODO WX, Window X Position, maybe we need to do some shit here
    | a >= 0xFF00 && a < 0xFF4C = error ((showHex a) ++ " = " ++ (showHex v) ++ " IO Register NOT IMPLEMENTED")
--    | a == 0xFFFF               = if v == 0 then writeRam else error ((showHex a) ++ " = " ++ (showHex v) ++ " IE Register NOT IMPLEMENTED")
    | a == 0xFFFF               = if v .&. 0x1C == 0 then writeRam else error ("$FFFF IE, Interrupt Enable, not allowed interrupt: " ++ show v)-- TODO IE, Interrupt Enable, maybe do some shit here
    | otherwise                 = writeRam

    where writeRam = (flip const) ("write " ++ showHex2 a ++ " " ++ showHex1 v) (m { memRam = (memRam m)//[(a, v)] })

