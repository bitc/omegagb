-- Copyright 2006 mutantlemon.com

module Joypad where

type JoypadKeyStates = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

--initJoypadKeyStates :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> JoypadKeyStates
initJoypadKeyStates right left up down a b select start =
  (right, left, up, down, a, b, select, start)

getJoypadKeyStateRight jp = let (v, _, _, _, _, _, _, _) = jp in v
getJoypadKeyStateLeft jp = let (_, v, _, _, _, _, _, _) = jp in v
getJoypadKeyStateUp jp = let (_, _, v, _, _, _, _, _) = jp in v
getJoypadKeyStateDown jp = let (_, _, _, v, _, _, _, _) = jp in v
getJoypadKeyStateA jp = let (_, _, _, _, v, _, _, _) = jp in v
getJoypadKeyStateB jp = let (_, _, _, _, _, v, _, _) = jp in v
getJoypadKeyStateSelect jp = let (_, _, _, _, _, _, v, _) = jp in v
getJoypadKeyStateStart jp = let (_, _, _, _, _, _, _, v) = jp in v

