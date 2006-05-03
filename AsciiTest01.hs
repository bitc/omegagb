-- Copyright 2006 mutantlemon.com

module AsciiTest01 where
import Prerequisites

import Data.Char
import Data.Array.IArray hiding ((!), (//))

import RomImage
import Machine
import Joypad

romFile = "roms/Catrap (U) [!].gb"

test01 :: IO ()
test01 = do
  romImage <- loadRomImage romFile
  let initialState = initialMachineState romImage

  let iter f x = let (r, x') = f x in
                 r : iter f x'

  let l = iter (updateMachineDisplayFrame (initJoypadKeyStates False False False False False False False False)) initialState

  let clear = putStr ((chr 27) : "[H")

  let pixel c = case c of
                0 -> " "
                1 -> "~"
                2 -> "="
                3 -> "@"

  let printRow d y = let s = concatMap (\x -> (pixel (d!(y, x)))) [0..159] in putStrLn s

  let printRows d = {- clear >> -} mapM_ (\y -> printRow d y) [0..138]

  mapM_ (\d -> printRows d) (take 200 l)

