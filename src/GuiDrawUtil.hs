-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  GuiDrawUtil
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- Code for updating a gtk+ canvas 
--
-----------------------------------------------------------------------------

module GuiDrawUtil where

-- Code shamelessly taken from gtk2hs fastdraw demo

import Graphics.UI.Gtk

updateCanvas :: DrawingArea -> Pixbuf -> Event -> IO Bool
updateCanvas canvas pb Expose { eventRegion = region } = do
  win <- drawingAreaGetDrawWindow canvas
  gc <- gcNew win
  width  <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  pbregion <- regionRectangle (Rectangle 0 0 width height)
  regionIntersect region pbregion
  rects <- regionGetRectangles region
--  putStrLn ("redrawing: "++show rects)
  (flip mapM_) rects $ \(Rectangle x y w h) -> do
    drawPixbuf win gc pb x y x y w h RgbDitherNone 0 0
  return True

{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from

