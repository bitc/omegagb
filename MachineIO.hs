-- Copyright 2006 mutantlemon.com

module MachineIO where

import Data.IORef
import Data.Array.IO
import Data.Word

import Machine

data MachineState = MachineState {
  msRegA :: IORef Word8,
  msRegB :: IORef Word8,
  msRegC :: IORef Word8,
  msRegD :: IORef Word8,
  msRegE :: IORef Word8,
  msRegF :: IORef Word8,
  msRegH :: IORef Word8,
  msRegL :: IORef Word8,
  msRegPC :: IORef Word16,
  msRegSP :: IORef Word16,
  msRam :: IOUArray Word16 Word8
}
  

