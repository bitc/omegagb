-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  TestRoms
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module generates some ROM images that can be used for testing
--
-----------------------------------------------------------------------------

module TestRoms where

import Data.Array.IO
import Data.Array.MArray
import Data.Word
import System.IO

import CpuExecution
import RomImage
import Machine

-- Creates a rom file that does incrementing and decrementing of the
-- A register
createTestRom01 :: FilePath -> IO ()
createTestRom01 file = do
    rom <- newArray (0, 0x7FFF) 0x00 :: IO (IOUArray Int Word8)
    writeArray rom 0x100 0x00 -- NOP
    writeArray rom 0x101 0xC3 -- JP 0x0150
    writeArray rom 0x102 0x50 --
    writeArray rom 0x103 0x01 --
    let nintendoGraphic = [
            0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00,
            0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89,
            0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB,
            0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F,
            0xBB, 0xB9, 0x33, 0x3E]
    mapM_ (\(i, e) -> writeArray rom i e) (zip [0x104..] nintendoGraphic)
    let title = map (fromIntegral . fromEnum) "OMEGAGBTEST01"
    mapM_ (\(i, e) -> writeArray rom i e) (zip [0x134..] title)
    -- TODO write header checksum

    -- now the actual program, starting at 0x150
    writeArray rom 0x150 0x3E -- LD A 0x00
    writeArray rom 0x151 0x00
    -- write alternating INC A (0x3C) / DEC A (0x3D) instructions
    mapM_ (\i -> writeArray rom i (if i `mod` 3 == 0 then 0x3C else 0x3D)) [0x152..0x7FFC]

    -- write a jump back to 0x150
    writeArray rom 0x7FFD 0xC3 -- JP 0x0150
    writeArray rom 0x7FFE 0x50
    writeArray rom 0x7FFF 0x01

    -- write the file
    fp <- openBinaryFile file WriteMode
    hPutArray fp rom 0x8000
    hClose fp

    return ()

runTestRom01 :: FilePath -> IO ()
runTestRom01 file = do
    rom <- loadRomImage file
    let machine = initialMachineState rom
    putStrLn ""
    putStrLn "Running 10,000 instructions"
    let ((registers, _), _) = (iterate updateMachine machine) !! 10000
    putStr "Value of A register (Should be 251): "
    putStrLn (show (getRegState registers M_A))

    return ()

