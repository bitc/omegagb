-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  MachineIO
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module is an alternative implementation of Machine, using the IO
-- monad and mutable variables in an attempt to get better performance.
-- I'm not sure if it works.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module MachineIO (msDisplay, initMachineStateIO, updateMachineDisplayFrameIO) where

import Data.Array.MArray
import Data.Bits
import Data.IORef
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)
import Data.Word
import Control.Monad.State

import Cpu
import CpuIO
import CpuExecution
import Machine
import MachineStateIO
import WordUtil

msRegIORef :: MachineStateIO -> M_Register -> IORef Word8
msRegIORef s reg = case reg of
  M_A -> msRegA s
  M_B -> msRegB s
  M_C -> msRegC s
  M_D -> msRegD s
  M_E -> msRegE s
  M_F -> msRegF s
  M_H -> msRegH s
  M_L -> msRegL s

msReg2IORef :: MachineStateIO -> M_Register2 -> IORef Word16
msReg2IORef s reg2 = case reg2 of
  M_PC -> msRegPC s
  M_SP -> msRegSP s

machineCpuExecuteIO :: MachineStateIO -> ExecutionAST () -> IO ()
machineCpuExecuteIO s e = machineCpuExecuteIO' s e

machineCpuExecuteIO' :: MachineStateIO -> ExecutionAST a -> IO a
machineCpuExecuteIO' s e = case e of
  Return result -> return result
  Bind l r -> do result <- machineCpuExecuteIO' s l
                 machineCpuExecuteIO' s (r result)
  WriteRegister reg n -> writeIORef (msRegIORef s reg) n
  ReadRegister reg -> readIORef (msRegIORef s reg)
  WriteRegister2 reg2 nn -> let (hi, lo) = splitWord16 nn in
                            case reg2 of
                              M_PC -> writeIORef (msRegPC s) nn
                              M_SP -> writeIORef (msRegSP s) nn
                              M_AF -> writeIORef (msRegA s) hi >>
                                      writeIORef (msRegF s) (lo.&.0xF0)
                              M_BC -> writeIORef (msRegB s) hi >>
                                      writeIORef (msRegC s) lo
                              M_DE -> writeIORef (msRegD s) hi >>
                                      writeIORef (msRegE s) lo
                              M_HL -> writeIORef (msRegH s) hi >>
                                      writeIORef (msRegL s) lo
  ReadRegister2 reg2 -> case reg2 of
                           M_PC -> readIORef (msRegPC s)
                           M_SP -> readIORef (msRegSP s)
                           M_AF -> do hi <- readIORef (msRegA s)
                                      lo <- readIORef (msRegF s)
                                      return $ joinWord16 hi lo
                           M_BC -> do hi <- readIORef (msRegB s)
                                      lo <- readIORef (msRegC s)
                                      return $ joinWord16 hi lo
                           M_DE -> do hi <- readIORef (msRegD s)
                                      lo <- readIORef (msRegE s)
                                      return $ joinWord16 hi lo
                           M_HL -> do hi <- readIORef (msRegH s)
                                      lo <- readIORef (msRegL s)
                                      return $ joinWord16 hi lo
  WriteMemory a n -> writeMemoryIO s a n
  ReadMemory a -> readMemoryIO s a

fetchInstructionIO :: MachineStateIO -> IO Instruction
fetchInstructionIO s = do
  pc <- readIORef (msRegPC s)
  opcode <- readMemoryIO s pc
  n <- readMemoryIO s (pc + 1)
  n' <- readMemoryIO s (pc + 2)
  let nn = joinWord16 n' n
  return $ machineCodeToInstruction opcode (n, nn)

machineStepInstructionIO :: MachineStateIO -> IO ()
machineStepInstructionIO s = do
  instruction <- fetchInstructionIO s
  let execution = executeInstruction instruction
  machineCpuExecuteIO s execution

updateMachineIO :: MachineStateIO -> IO ()
updateMachineIO s = do
  pc <- readIORef (msRegPC s)
  opcode <- readMemoryIO s pc
  let cycles = opcodeCycleCount opcode
--  let ime = opcodeQueryIME opcode
  --machineStepInstructionIO s
  machineUpdateInstructionIO s
  irqUpdateIO s cycles --ime

updateMachineDisplayFrameIO :: MachineStateIO -> IO ()
updateMachineDisplayFrameIO s = do
  updateMachineIO s
  vbn <- readIORef (msVBlankNow s)
  case vbn of
    True -> return ()
    False -> updateMachineDisplayFrameIO s

irqUpdateIO :: MachineStateIO -> CycleCount -> IO() --Maybe Bool -> IO ()
irqUpdateIO s cycles = do --ime = do
--  when (isJust ime) (writeIORef (msIME s) (fromJust ime))

  let subtractCycles x = modifyIORef (x s) (subtract cycles)
  mapM_ subtractCycles [msVBlankCounter, msHBlankCounter, msHBlankMode3Counter,
                        msHBlankMode0Counter, msDIVCounter]

  v <- readIORef (msVBlankCounter s)
  writeIORef (msVBlankNow s) (v <= 0)

  let processCounter x y z = do c <- readIORef (x s)
                                when (c <= 0)
                                     (writeIORef (x s) (c + y) >> z s)

  processCounter msHBlankCounter      hBlankPeriod tickHBlankIO
  processCounter msHBlankMode3Counter hBlankPeriod tickHBlankMode3IO
  processCounter msHBlankMode0Counter hBlankPeriod tickHBlankMode0IO
  processCounter msVBlankCounter      vBlankPeriod tickVBlankIO
  processCounter msDIVCounter         divPeriod    tickDIVIO

  ime <- readIORef (msIME s)
  flagsIF <- readMemoryIO s 0xFF0F
  flagsIE <- readMemoryIO s 0xFFFF

  when (ime && (flagsIF .&. flagsIE > 0))
    (
       let i = getLowBit (flagsIF .&. flagsIE)
           jumpAddr = case i of
                        0 -> 0x0040
                        1 -> 0x0048
                        2 -> 0x0050
                        3 -> 0x0058
                        4 -> 0x0060 in
       do writeIORef (msIME s) False
          modifyArray (msRam s) 0xFF0F (`clearBit` i)
          oldPC <- readIORef (msRegPC s)
          let (hiPC, loPC) = splitWord16 oldPC
          oldSP <- readIORef (msRegSP s)
          writeMemoryIO s (oldSP - 1) hiPC
          writeMemoryIO s (oldSP - 2) loPC
          writeIORef (msRegSP s) (oldSP - 2)
          writeIORef (msRegPC s) jumpAddr
    )

  where getLowBit :: Word8 -> Int
        getLowBit n = fromJust (elemIndex True (map (testBit n) [0..4]))

modifyArray a i f = readArray a i >>= ( \v -> writeArray a i (f v) )

tickHBlankIO :: MachineStateIO -> IO ()
tickHBlankIO s = do
  modifyArray (msRam s) 0xFF44 ((`mod` 154).(+1))
  modifyIORef (msCurrentScanline s) ((`mod` 154).(+1))
  cs <- readIORef (msCurrentScanline s)
  when (cs < 144)
    ( do
      ly  <- readArray (msRam s) 0xFF44
      lyc <- readArray (msRam s) 0xFF45
      statB5 <- readArray (msRam s) 0xFF41 >>= return . (`testBit` 5)
      statB6 <- readArray (msRam s) 0xFF41 >>= return . (`testBit` 6)
      modifyArray (msRam s) 0xFF41 (execState $ do
                                      modify (`clearBit` 0)
                                      modify (`setBit` 1)
                                      modify (if ly==lyc then (`setBit` 2) else (`clearBit` 2)))
      modifyArray (msRam s) 0xFF0F (execState $ when (statB5 || (statB6 && ly==lyc)) (modify (`setBit` 1)))
      readIORef (msCurrentScanline s) >>= renderScanLineIO s
    )

tickHBlankMode3IO :: MachineStateIO -> IO ()
tickHBlankMode3IO s = do
  cs <- readIORef (msCurrentScanline s)
  when (cs < 144)
    ( do
      modifyArray (msRam s) 0xFF41 (execState $ do
                                      modify (`setBit` 0)
                                      modify (`setBit` 1))
    )

tickHBlankMode0IO :: MachineStateIO -> IO ()
tickHBlankMode0IO s = do
  cs <- readIORef (msCurrentScanline s)
  when (cs < 144)
    ( do
      statB3 <- readArray (msRam s) 0xFF41 >>= return . (`testBit` 3)
      modifyArray (msRam s) 0xFF41 (execState $ do
                                      modify (`clearBit` 0)
                                      modify (`clearBit` 1))
      modifyArray (msRam s) 0xFF0F (execState $ when statB3 (modify (`setBit` 1)))
    )

tickVBlankIO :: MachineStateIO -> IO ()
tickVBlankIO s = do
  statB4 <- readArray (msRam s) 0xFF41 >>= return . (`testBit` 4)
  modifyArray (msRam s) 0xFF41 (execState $ do
                                  modify (`setBit` 0)
                                  modify (`clearBit` 1))
  modifyArray (msRam s) 0xFF0F (execState $ do
                                  when statB4 (modify (`setBit` 1))
                                  modify (`setBit` 0))

tickDIVIO :: MachineStateIO -> IO ()
tickDIVIO s = modifyArray (msRam s) 0xFF04 (+1)

renderScanLineIO s scanlineNum = return ()
{-
renderScanLineIO :: MachineStateIO -> Int -> IO ()
renderScanLineIO s scanlineNum = let scanline = (msDisplay s)!scanlineNum in do
  scx <- readMemoryIO s 0xFF43 >>= return . fromIntegral
  scy <- readMemoryIO s 0xFF42 >>= return . fromIntegral
  lcdc <- readMemoryIO s 0xFF40
  let lcdon = testBit lcdc 7
  let bgon = testBit lcdc 0
  let bgmap = testBit lcdc 3
  let bgmapStartAddr = if bgmap then 0x9C00 else 0x9800
  let bgtiles = testBit lcdc 4
  let bgtilesStartAddr = if bgtiles then 0x8000 else 0x9000
  let spriteson = testBit lcdc 1
  let spritesbig = testBit lcdc 2
  let getBgPixel :: Int -> Int -> IO Word8
      getBgPixel x y = do
        let x' = (x + scx) `mod` 256
            y' = (y + scy) `mod` 256
            xrow = x' `div` 8
            yrow = y' `div` 8
            tileNum = fromIntegral (yrow * 32 + xrow)
        tileIndex <- readMemoryIO s (tileNum + bgmapStartAddr)
        let tileStartMem = bgtilesStartAddr + (16 * (fromIntegral tileIndex))
            xoff = 7 - (x' `mod` 8)
            yoff = y' `mod` 8
            hiByte = tileStartMem + (yoff * 2)
            loByte = tileStartMem + (yoff * 2) + 1
        hiByteValue <- readMemoryIO s (fromIntegral hiByte)
        loByteValue <- readMemoryIO s (fromIntegral loByte)
        let color = (2 * (fromEnum (testBit loByteValue xoff))) +
                         (fromEnum (testBit hiByteValue xoff))
        if (bgon && lcdon)
           then return (fromIntegral color)
           else return 0

  mapM_ ( \x -> getBgPixel x scanlineNum >>= writeArray scanline x ) [0..159]
-}


