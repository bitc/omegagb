-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  CpuExecution
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module defines an abstract syntax tree CPU execution monad
--
-----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}

module CpuExecution where

import Data.Word
import Data.Bits

data M_Register =
  M_A |
  M_B |
  M_C |
  M_D |
  M_E |
  M_F |
  M_H |
  M_L

data M_Register2 =
  M_AF |
  M_BC |
  M_DE |
  M_HL |
  M_PC |
  M_SP

data ExecutionAST result where
  Return :: result -> ExecutionAST result
  Bind :: (ExecutionAST oldres) -> (oldres -> ExecutionAST result) ->
          ExecutionAST result
  WriteRegister :: M_Register -> Word8 -> ExecutionAST ()
  ReadRegister :: M_Register -> ExecutionAST Word8
  WriteRegister2 :: M_Register2 -> Word16 -> ExecutionAST ()
  ReadRegister2 :: M_Register2 -> ExecutionAST Word16
  WriteMemory :: Word16 -> Word8 -> ExecutionAST ()
  ReadMemory :: Word16 -> ExecutionAST Word8
instance Monad ExecutionAST where
  return = Return
  (>>=) = Bind

writeRegister = WriteRegister
readRegister = ReadRegister
writeRegister2 = WriteRegister2
readRegister2 = ReadRegister2
writeMemory = WriteMemory
readMemory = ReadMemory

writeFlags :: Maybe Bool ->
              Maybe Bool ->
              Maybe Bool ->
              Maybe Bool ->
              ExecutionAST ()
writeFlags z n h c = do
  v0 <- readRegister M_F
  let v1 = case z of
             Nothing -> v0
             Just True -> setBit v0 7
             Just False -> clearBit v0 7
  let v2 = case n of
             Nothing -> v1
             Just True -> setBit v1 6
             Just False -> clearBit v1 6
  let v3 = case h of
             Nothing -> v2
             Just True -> setBit v2 5
             Just False -> clearBit v2 5
  let v4 = case c of
             Nothing -> v3
             Just True -> setBit v3 4
             Just False -> clearBit v3 4
  writeRegister M_F v4

