-- Copyright 2006 mutantlemon.com

module MachineStateIO where

import Data.Bits
import Data.IORef
import Data.Word
import Data.Array.Base
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.IO

import RomImage
import WordUtil

type DisplayPixel = Word8

type ScanLine = IOUArray Int DisplayPixel

data MachineStateIO = MachineStateIO {
  msRegA :: (IORef Word8),
  msRegB :: (IORef Word8),
  msRegC :: (IORef Word8),
  msRegD :: (IORef Word8),
  msRegE :: (IORef Word8),
  msRegF :: (IORef Word8),
  msRegH :: (IORef Word8),
  msRegL :: (IORef Word8),
  msRegPC :: (IORef Word16),
  msRegSP :: (IORef Word16),
  msRam :: (IOUArray Int Word8),
  msRomImage :: RomImage,
  msIME :: (IORef Bool),
  msVBlankCounter :: (IORef Int),
  msHBlankCounter :: (IORef Int),
  msHBlankMode3Counter :: (IORef Int),
  msHBlankMode0Counter :: (IORef Int),
  msCurrentScanline :: (IORef Int),
  msDisplay :: (Array Int ScanLine),
  msVBlankNow :: (IORef Bool),
  msDIVCounter :: (IORef Int)
}

initMachineStateIO :: RomImage -> IO MachineStateIO
initMachineStateIO romImage = do
  a <- newIORef 0x01
  b <- newIORef 0x00
  c <- newIORef 0x13
  d <- newIORef 0x00
  e <- newIORef 0xD8
  f <- newIORef 0xB0
  h <- newIORef 0x01
  l <- newIORef 0x4D
  pc <- newIORef 0x0100
  sp <- newIORef 0xFFFE
  ram <- newArray (0x8000, 0xFFFF) 0x00
  writeArray ram 0xFF00 0x30
  writeArray ram 0xFF05 0x00
  writeArray ram 0xFF06 0x00
  writeArray ram 0xFF07 0x00
  writeArray ram 0xFF10 0x80
  writeArray ram 0xFF11 0xBF
  writeArray ram 0xFF12 0xF3
  writeArray ram 0xFF14 0xBF
  writeArray ram 0xFF16 0x3F
  writeArray ram 0xFF17 0x00
  writeArray ram 0xFF19 0xBF
  writeArray ram 0xFF1A 0x7F
  writeArray ram 0xFF1B 0xFF
  writeArray ram 0xFF1C 0x9F
  writeArray ram 0xFF1E 0xBF
  writeArray ram 0xFF20 0xFF
  writeArray ram 0xFF21 0x00
  writeArray ram 0xFF22 0x22
  writeArray ram 0xFF23 0xBF
  writeArray ram 0xFF24 0x77
  writeArray ram 0xFF25 0xF3
  writeArray ram 0xFF26 0xF1
  writeArray ram 0xFF40 0x91
  writeArray ram 0xFF42 0x00
  writeArray ram 0xFF43 0x00
  writeArray ram 0xFF45 0x00
  writeArray ram 0xFF47 0xFC
  writeArray ram 0xFF48 0xFF
  writeArray ram 0xFF49 0xFF
  writeArray ram 0xFF4A 0x00
  writeArray ram 0xFF4B 0x00
  writeArray ram 0xFFFF 0x00
  ime <- newIORef False
  vBlankCounter <- newIORef 0
  hBlankCounter <- newIORef 0
  hBlankMode3Counter <- newIORef 80
  hBlankMode0Counter <- newIORef (80 + 172)
  currentScanline <- newIORef 153
  associations <- mapM ( \n -> do { a <- newArray (0, 159) 0; return (n, a) } ) [0..143]
  let display = array (0, 143) associations
  vBlankNow <- newIORef False
  divCounter <- newIORef 0
  return MachineStateIO {
    msRegA = a,
    msRegB = b,
    msRegC = c,
    msRegD = d,
    msRegE = e,
    msRegF = f,
    msRegH = h,
    msRegL = l,
    msRegPC = pc,
    msRegSP = sp,
    msRam = ram,
    msRomImage = romImage,
    msIME = ime,
    msVBlankCounter = vBlankCounter,
    msHBlankCounter = hBlankCounter,
    msHBlankMode3Counter = hBlankMode3Counter,
    msHBlankMode0Counter = hBlankMode0Counter,
    msCurrentScanline = currentScanline,
    msDisplay = display,
    msVBlankNow = vBlankNow,
    msDIVCounter = divCounter }


{-# INLINE readMemoryIO #-}
readMemoryIO :: MachineStateIO -> Word16 -> IO Word8
readMemoryIO s a
  | a < 0x8000  = return $ (msRomImage s) ! (fromIntegral a)
  | a == 0xFF00 = return 0x00
  | otherwise   = unsafeRead (msRam s) (fromIntegral a)
--  where m = msRam s
--        readRam = readArray m a

{-# INLINE writeMemoryIO #-}
writeMemoryIO :: MachineStateIO -> Word16 -> Word8 -> IO ()
writeMemoryIO s a v
  | a < 0x8000  = return ()
  | a == 0xF000 = if v == 0x00 || v == 0x20 || v == 0x10 || v == 0x30
                  then writeRam
                  else error ("$FF00 P1, Joypad, not allowed: " ++ show v)
  | a == 0xF004 = writeArray m (fromIntegral a) 0x00
  | a == 0xFF07 = if not (testBit v 2)
                  then writeRam
                  else error ("$FF07 TAC, Timer Control Register, not allowed: " ++ show v)
  | a == 0xFF44 = writeArray m (fromIntegral a) 0x00
  | a == 0xFF46 = mapM_ ( \i -> do srcVal <- readArray m (fromIntegral ((i-0xFE00) + (joinWord16 v 0)))
                                   writeArray m (fromIntegral i) srcVal )
                        [0xFE00..0xFE9f]
--  | a >= 0xFF00 && a < 0xFF4C = error ((showHex a) ++ " = " ++ (showHex v) ++ " IO Register NOT IMPLEMENTED")
  | a == 0xFFFF = if v .&. 0x1C == 0 then writeRam else error ("$FFFF IE, Interrupt Enable, not allowed interrupt: " ++ show v)-- TODO IE, Interrupt Enable, maybe do some shit here
  | otherwise   = writeRam
  where m = msRam s
        writeRam = writeArray m (fromIntegral a) v


