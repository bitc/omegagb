-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Tests
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- Some really basic tests. From the early life of the project.
-- Not really useful anymore
--
-----------------------------------------------------------------------------

module Tests where

import Data.Array.IArray
import Data.Word

import Machine
import Cpu
import CpuExecution
import RomImage
import Memory
import WordUtil

cpuTest :: Int -> Bool

type ReallySimpleMemory = Array Word16 Word8

instance MemoryModel ReallySimpleMemory where
  readMem = readReallySimpleMemory
  writeMem = writeReallySimpleMemory

initialReallySimpleMemoryMachineState =
  (initialRegisterStates, zeroReallySimpleMemory)

zeroReallySimpleMemory :: ReallySimpleMemory
zeroReallySimpleMemory = listArray (0x0000, 0xFFFF) (replicate (0xFFFF+1) 0x00)

readReallySimpleMemory :: ReallySimpleMemory -> Word16 -> Word8
readReallySimpleMemory m a = m!a

writeReallySimpleMemory :: ReallySimpleMemory -> Word16 -> Word8 -> ReallySimpleMemory
writeReallySimpleMemory m a v = m//[(a, v)]

-- LD A,1
-- LD (0),A
cpuTest 1 =
  let s0 = initialReallySimpleMemoryMachineState
      s1 = machineCpuExecute s0 (executeInstruction (LDRN A 0x9B))
      s2 = machineCpuExecute s1 (executeInstruction (LDPN 0x0000))
      (_, mem) = s2
      m0 = readReallySimpleMemory mem 0x0000 in
  m0 == 0x9B

-- LD BC,0xFAEB
-- PUSH BC
-- POP AF
-- LD (0),A
cpuTest 2 =
  let s0 = initialReallySimpleMemoryMachineState
      s1 = machineCpuExecute s0 (executeInstruction (LD2 BC 0xFAEB))
      s2 = machineCpuExecute s1 (executeInstruction (PUSH StackRegBC))
      s3 = machineCpuExecute s2 (executeInstruction (POP StackRegAF))
      s4 = machineCpuExecute s3 (executeInstruction (LDPN 0))
      (_, mem) = s4
      m0 = readReallySimpleMemory mem 0 in
  m0 == 0xFA

-- LD SP,0x0002
-- LD BC,0xFAEB
-- PUSH BC
cpuTest 3 =
  let s0 = initialReallySimpleMemoryMachineState
      s1 = machineCpuExecute s0 (executeInstruction (LD2 SP 0x0002))
      s2 = machineCpuExecute s1 (executeInstruction (LD2 BC 0xFAEB))
      s3 = machineCpuExecute s2 (executeInstruction (PUSH StackRegBC))
      (_, mem) = s3
      m0 = readReallySimpleMemory mem 0x0000
      m1 = readReallySimpleMemory mem 0x0001 in
  (m0 == 0xEB) && (m1 == 0xFA)
 
-- LD BC 0xFA45
cpuTest 4 =
  let s0 = initialReallySimpleMemoryMachineState
      s1 = machineCpuExecute s0 (executeInstruction (LD2 BC 0xFA45))
      (regs, _) = s1
      b = getRegState regs M_B in
  b == 0xFA

romFile = "roms/Loopz (U).gb"

fetchPC :: (MemoryModel m) => (RegisterStates, m) -> Word16
fetchPC (regS, _) = getReg2State regS M_PC

fetchA :: (MemoryModel m) => (RegisterStates, m) -> Word8
fetchA (regS, _) = getRegState regS M_A

romExecutionTest :: Int -> IO ()
romExecutionTest 1 = do
  romImage <- loadRomImage romFile
  let l = iterate updateMachine ((initialRegisterStates, initMemory romImage),
                                 initialIrqStates)
  let dis = map (\s ->
                   (showHex (fetchPC s)) ++ " " ++ (show (fetchInstruction s)) ++ " " ++ (showHex (fetchA s))
                ) (map fst l)
  mapM_ (\s -> putStr s >> wait) dis

  where wait = putStrLn "" --getChar

