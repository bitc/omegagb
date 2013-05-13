-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Joypad
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module defines the type for the Game Boy joypad, which is basiclly a
-- collection of push buttons.
--
-----------------------------------------------------------------------------

module Joypad where

data JoypadKeyStates = JoypadKeyStates !Bool !Bool !Bool !Bool !Bool !Bool !Bool !Bool

--initJoypadKeyStates :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> JoypadKeyStates
initJoypadKeyStates right left up down a b select start =
  JoypadKeyStates right left up down a b select start

getJoypadKeyStateRight jp = let JoypadKeyStates v _ _ _ _ _ _ _ = jp in v
getJoypadKeyStateLeft jp = let JoypadKeyStates _ v _ _ _ _ _ _ = jp in v
getJoypadKeyStateUp jp = let JoypadKeyStates _ _ v _ _ _ _ _ = jp in v
getJoypadKeyStateDown jp = let JoypadKeyStates _ _ _ v _ _ _ _ = jp in v
getJoypadKeyStateA jp = let JoypadKeyStates _ _ _ _ v _ _ _ = jp in v
getJoypadKeyStateB jp = let JoypadKeyStates _ _ _ _ _ v _ _ = jp in v
getJoypadKeyStateSelect jp = let JoypadKeyStates _ _ _ _ _ _ v _ = jp in v
getJoypadKeyStateStart jp = let JoypadKeyStates _ _ _ _ _ _ _ v = jp in v

