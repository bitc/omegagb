-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  GuiTest02
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module runs a gtk+ application that emulates a ROM, including a
-- display and push buttons for joypad control.
--
-----------------------------------------------------------------------------

module GuiTest02 where

import qualified Control.Exception as C
import Data.Maybe(fromJust)
import Data.IORef
import Data.Word
import Data.Array.IArray
import Data.Array.MArray
import Text.Printf

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import RomImage
import GuiDrawUtil
import Machine
import Joypad
--import MachineIO

gladeFile = "guis/test03/test03.glade"

test02 :: IO ()
test02 = do
  initGUI

  windowXml <- C.catch
               ((xmlNew gladeFile) >>= return . fromJust)
               (\e -> putStrLn ("Error Loading " ++ gladeFile) >> C.throwIO (e :: C.IOException))

  let bindWidget x y = xmlGetWidget windowXml x y
  main_window   <- bindWidget castToWindow       "main_window"
  menu_open     <- bindWidget castToMenuItem     "menu_open"
  menu_quit     <- bindWidget castToMenuItem     "menu_quit"
  menu_about    <- bindWidget castToMenuItem     "menu_about"
  display       <- bindWidget castToDrawingArea  "display"
  joypad_right  <- bindWidget castToToggleButton "joypad_right"
  joypad_left   <- bindWidget castToToggleButton "joypad_left"
  joypad_up     <- bindWidget castToToggleButton "joypad_up"
  joypad_down   <- bindWidget castToToggleButton "joypad_down"
  joypad_a      <- bindWidget castToToggleButton "joypad_a"
  joypad_b      <- bindWidget castToToggleButton "joypad_b"
  joypad_select <- bindWidget castToToggleButton "joypad_select"
  joypad_start  <- bindWidget castToToggleButton "joypad_start"

  displayPixBuf <- pixbufNew ColorspaceRgb False 8 160 144
  pbData <- (pixbufGetPixels displayPixBuf :: IO (PixbufData Int Word8))
  row <- pixbufGetRowstride displayPixBuf
  chan <- pixbufGetNChannels displayPixBuf
  bits <- pixbufGetBitsPerSample displayPixBuf

  state <- newIORef Nothing

  -- for video capture, counts the current frame number
  --n <- newIORef (0::Int)

  let
      ------------------------------------------------------------------------

      refreshDisplay d = do
        -- draw into the Pixbuf
        doFromTo 0 143 $ \y ->
          doFromTo 0 159 $ \x -> do
            let color = d!(y, x)
                colorByte = (fromIntegral color) * 85
            writeArray pbData (x*chan+y*row) colorByte
            writeArray pbData (1+x*chan+y*row) colorByte
            writeArray pbData (2+x*chan+y*row) colorByte

        widgetQueueDraw display

      ------------------------------------------------------------------------

      step = do
        s <- readIORef state
        case s of
          Nothing -> return ()
          Just s' -> do
            right <- toggleButtonGetActive joypad_right
            left <- toggleButtonGetActive joypad_left
            up <- toggleButtonGetActive joypad_up
            down <- toggleButtonGetActive joypad_down
            a <- toggleButtonGetActive joypad_a
            b <- toggleButtonGetActive joypad_b
            select <- toggleButtonGetActive joypad_select
            start <- toggleButtonGetActive joypad_start
            let jp = initJoypadKeyStates right left up down a b select start
            let (d, s'') = updateMachineDisplayFrame jp s'
            writeIORef state (Just s'')
            refreshDisplay d
            --- for video capture, dump current frame to png file
            --num <- readIORef n
            --pixbufSave displayPixBuf ("tmp/f" ++ (printf "%04d" num) ++ ".png") "png" []
            --modifyIORef n (+1)
            ---
        return True

      ------------------------------------------------------------------------

      ------------------------------------------------------------------------

      open = do
        fileSelect <- fileChooserDialogNew
                        (Just "Open Game Boy ROM")
                        (Just main_window)
                        FileChooserActionOpen
                        [("gtk-open", ResponseOk), ("gtk-cancel", ResponseDeleteEvent)]
        response <- dialogRun fileSelect
        case response of
          ResponseOk -> do
            romFile <- fileChooserGetFilename fileSelect
            romImage <- loadRomImage (fromJust romFile)
            writeIORef state $ Just (initialMachineState romImage)
          ResponseDeleteEvent -> do
            return ()
        widgetDestroy fileSelect

        -- register Idle action

      ------------------------------------------------------------------------

      quit = widgetDestroy main_window >> mainQuit

      ------------------------------------------------------------------------

  menu_quit `onActivateLeaf` quit
  main_window `onDestroy` quit
  menu_open `onActivateLeaf` open
  menu_about `onActivateLeaf` do
    dia <- aboutDialogNew
    aboutDialogSetName dia "OmegaGB test01"
    aboutDialogSetComments dia "Game Boy Emulator Development Test"
    dialogRun dia
    widgetDestroy dia

  display `onSizeRequest` return (Requisition 160 144)
  display `onExpose` updateCanvas display displayPixBuf

  idleAdd step priorityDefaultIdle

  mainGUI
  return ()

