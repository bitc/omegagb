-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  RomImage
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module defines the type for a ROM image: the contents of a dumped
-- Game Boy game cartdrige.
--
-----------------------------------------------------------------------------

module RomImage where

import System.Posix.Files (getFileStatus, fileSize)
import System.IO
import Control.Monad (when)
import Control.Exception (bracket)
import Data.Array.Unboxed
import Data.Word
import Data.Char (chr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Storable (peekElemOff)

type RomImage = UArray Int Word8

readRomImageByte :: RomImage -> Int -> Word8
readRomImageByte ri a = ri!a

loadRomImage :: FilePath -> IO RomImage
loadRomImage file = do
  fileStatus <- getFileStatus file
  let size = fromIntegral (fileSize fileStatus)

  a <- bracket
         (openBinaryFile file ReadMode)
         (hClose)
         (\handle -> do
            bracket
              (mallocBytes size)
              (free)
              (\buf -> do
                 numBytesRead <- hGetBuf handle buf size
                 when (numBytesRead /= size) (error "TODO error")
                 let readIndex :: Int -> IO (Int, Word8)
                     readIndex i = do
                       v <- peekElemOff buf (fromIntegral i)
                       return (i, v)

                 resultList <- mapM readIndex [0..(size-1)]

                 let result :: RomImage
                     result = array (0, size-1) resultList

                 return result
              )
         )

  putStrLn "RomImage Loaded Succesfully:"
  putStrLn file
  putStrLn $ "Title: " ++ (map (\i -> chr (fromIntegral (a!i)))
                               [0x0134..0x0142])
  putStrLn $ "Cartridge type: " ++ case a!0x0147 of
    0x00 -> "ROM ONLY"
    0x01 -> "ROM+MBC1"
    0x02 -> "ROM+MBC1+RAM"
    0x03 -> "ROM+MBC1+RAM+BATT"
    0x05 -> "ROM+MBC2"
    0x06 -> "ROM+MBC2+BATTERY"
    0x08 -> "ROM+RAM"
    0x09 -> "ROM+RAM+BATTERY"
    0x0B -> "ROM+MMM01"
    0x0C -> "ROM+MMM01+SRAM"
    0x0D -> "ROM+MMM01+SRAM+BATT"
    0x0F -> "ROM+MBC3+TIMER+BATT"
    0x10 -> "ROM+MBC3+TIMER+RAM+BATT"
    0x11 -> "ROM+MBC3"
    0x12 -> "ROM+MBC3+RAM"
    0x13 -> "ROM+MBC3+RAM+BATT"
    0x19 -> "ROM+MBC5"
    0x1A -> "ROM+MBC5+RAM"
    0x1B -> "ROM+MBC5+RAM+BATT"
    0x1C -> "ROM+MBC5+RUMBLE"
    0x1D -> "ROM+MBC5+RUMBLE+SRAM"
    0x1E -> "ROM+MBC5+RUMBLE+SRAM+BATT"
    0x1F -> "Pocket Camera"
    0xFD -> "Bandai TAMA5"
    0xFE -> "Hudson HuC-3"
    0xFF -> "Hudson HuC-1"
    _ -> "INVALID OR UKNOWN"
  putStrLn $ "ROM Size: " ++ case a!0x0148 of
    0x00 -> "256Kbit = 32KByte = 2 banks"
    0x01 -> "512Kbit = 64KByte = 4 banks"
    0x02 -> "1Mbit = 128KByte = 8 banks"
    0x03 -> "2Mbit = 256KByte = 16 banks"
    0x04 -> "4Mbit = 512KByte = 32 banks"
    0x05 -> "8Mbit = 1MByte = 64 banks"
    0x06 -> "16Mbit = 2MByte = 128 banks"
    0x52 -> "9Mbit = 1.1MByte = 72 banks"
    0x53 -> "10Mbit = 1.2MByte = 80 banks"
    0x54 -> "12Mbit = 1.5MByte = 96 banks"
    _ -> "INVALID OR UKNOWN"
  putStrLn $ "RAM Size: " ++ case a!0x0149 of
    0x00 -> "None"
    0x01 -> "16kBit = 2kB = 1 bank"
    0x02 -> "64kBit = 8kB = 1 bank"
    0x03 -> "256kBit = 32kB = 4 banks"
    0x04 -> "1MBit = 128kB = 16 banks"
    _ -> "INVALID OR UKNOWN"
  putStrLn $ "Destination code: " ++ case a!0x014A of
    0x00 -> "Japanese"
    0x01 -> "Non-Japanese"
    _ -> "INVALID OR UKNOWN"
  putStrLn $ "Licensee code: " ++ case a!0x014B of
    0x79 -> "Accolade"
    0xA4 -> "Konami"
    0x33 -> [chr (fromIntegral (a!0x0144)), chr (fromIntegral (a!0x0145))]
    _ -> "INVALID OR UNKOWN"
  putStrLn $ "Mask ROM Version number: " ++ (show (a!0x014C))

  return a

