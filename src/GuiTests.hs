-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  GuiTests
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module runs a gtk+ application that is some sort of Game Boy
-- debugger. It allows you to step through instructions and view the values
-- of registers, and graphics memory.
--
-----------------------------------------------------------------------------

module GuiTests where

import Maybe(fromJust)
import qualified Control.Exception as C
import Data.IORef
import Data.Bits
import Control.Monad
import Data.Array.MArray
import Data.Word
import Data.Int

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import WordUtil
import Machine
import Memory
import RomImage
import CpuExecution
import GuiDrawUtil

type State = Maybe (((RegisterStates, Memory), IrqStates), Maybe HandlerId)

test01 :: IO ()
test01 = do
  initGUI

  windowXml <- C.catch
               ((xmlNew gladeFile) >>= return . fromJust)
               (\e -> putStrLn ("Error Loading " ++ gladeFile) >> C.throwIO e)

  return ()

  let bindWidget x y = xmlGetWidget windowXml x y
  window_main <- bindWidget castToWindow "window_main"
  menu_open <- bindWidget castToMenuItem "menu_open"
  menu_quit <- bindWidget castToMenuItem "menu_quit"
  menu_step <- bindWidget castToMenuItem "menu_step"
  menu_run <- bindWidget castToMenuItem "menu_run"
  menu_pause <- bindWidget castToMenuItem "menu_pause"
  menu_about <- bindWidget castToMenuItem "menu_about"
  button_open <- bindWidget castToToolButton "button_open"
  button_step <- bindWidget castToToolButton "button_step"
  button_run <- bindWidget castToToolButton "button_run"
  button_pause <- bindWidget castToToolButton "button_pause"
  reg_a <- bindWidget castToEntry "reg_a"
  reg_b <- bindWidget castToEntry "reg_b"
  reg_c <- bindWidget castToEntry "reg_c"
  reg_d <- bindWidget castToEntry "reg_d"
  reg_e <- bindWidget castToEntry "reg_e"
  reg_f <- bindWidget castToEntry "reg_f"
  reg_h <- bindWidget castToEntry "reg_h"
  reg_l <- bindWidget castToEntry "reg_l"
  reg_pc <- bindWidget castToEntry "reg_pc"
  reg_sp <- bindWidget castToEntry "reg_sp"
  flag_ime <- bindWidget castToCheckButton "flag_ime"
  flag_z <- bindWidget castToEntry "flag_z"
  flag_n <- bindWidget castToEntry "flag_n"
  flag_h <- bindWidget castToEntry "flag_h"
  flag_c <- bindWidget castToEntry "flag_c"
  reg_ie <- bindWidget castToEntry "reg_ie"
  reg_stat <- bindWidget castToEntry "reg_stat"
  dissassembler_textview <- bindWidget castToTextView "dissassembler_textview"
  main_notebook <- bindWidget castToNotebook "main_notebook"
  map_refresh <- bindWidget castToButton "map_refresh"
  map_selector <- bindWidget castToComboBox "map_selector"
  map_drawingarea <- bindWidget castToDrawingArea "map_drawingarea"

  mapPixBuf <- pixbufNew ColorspaceRgb False 8 256 256

  state <- newIORef (Nothing::State)

  let setStepSensitivity s = mapM_ (`widgetSetSensitivity` s) [toWidget button_step, toWidget menu_step]
      setRunSensitivity s = mapM_ (`widgetSetSensitivity` s) [toWidget button_run, toWidget menu_run]
      setPauseSensitivity s = mapM_ (`widgetSetSensitivity` s) [toWidget button_pause, toWidget menu_pause]

      ------------------------------------------------------------------------

      updateRunCommandsSensitivity = do
        s <- readIORef state
        case s of
          Nothing -> do
                     setStepSensitivity False
                     setRunSensitivity False
                     setPauseSensitivity False
          Just (_, Nothing) -> do
                             setStepSensitivity True
                             setRunSensitivity True
                             setPauseSensitivity False
          Just (_, Just _) -> do
                            setStepSensitivity False
                            setRunSensitivity False
                            setPauseSensitivity True

      ------------------------------------------------------------------------
 
      updateDebugPanel = do
        s <- readIORef state
        case s of
          Nothing -> return ()
          Just (((regS, memS), irqS), _) -> do
            reg_a `entrySetText` showHex1 (getRegState regS M_A)
            reg_b `entrySetText` showHex1 (getRegState regS M_B)
            reg_c `entrySetText` showHex1 (getRegState regS M_C)
            reg_d `entrySetText` showHex1 (getRegState regS M_D)
            reg_e `entrySetText` showHex1 (getRegState regS M_E)
            reg_f `entrySetText` showHex1 (getRegState regS M_F)
            reg_h `entrySetText` showHex1 (getRegState regS M_H)
            reg_l `entrySetText` showHex1 (getRegState regS M_L)
            reg_pc `entrySetText` showHex2 (getReg2State regS M_PC)
            reg_sp `entrySetText` showHex2 (getReg2State regS M_SP)
            reg_ie `entrySetText` showHex1 (readMem memS 0xFFFF)
            --reg_stat `entrySetText` showHex1 (readMem memS 0xFF41)
            flag_ime `toggleButtonSetActive` irqStateIME irqS
            flag_z `entrySetText` show (fromEnum (testBit (getRegState regS M_F) 7))
            flag_n `entrySetText` show (fromEnum (testBit (getRegState regS M_F) 6))
            flag_h `entrySetText` show (fromEnum (testBit (getRegState regS M_F) 5))
            flag_c `entrySetText` show (fromEnum (testBit (getRegState regS M_F) 4))

      ------------------------------------------------------------------------
            
      displayCurrentInstruction = do
        s <- readIORef state
        case s of
          Nothing -> return ()
          Just (((regS, memS), _), _) -> do
            let pc = getReg2State regS M_PC
            let instruction = fetchInstruction (regS, memS)
            let s = (showHex2 pc) ++ " " ++ (show instruction) ++ "\n"
            buffer <- textViewGetBuffer dissassembler_textview
            n <- textBufferGetLineCount buffer
            when (n > instructionHistoryCount)
                 (do iterStart <- textBufferGetStartIter buffer
                     iter1 <- textBufferGetIterAtLine buffer 1
                     textBufferDelete buffer iterStart iter1)
            endIter <- textBufferGetEndIter buffer
            textBufferInsert buffer endIter s

      ------------------------------------------------------------------------

      clearInstructionDisplay = do
        buffer <- textViewGetBuffer dissassembler_textview 
        startIter <- textBufferGetStartIter buffer
        endIter <- textBufferGetEndIter buffer
        textBufferDelete buffer startIter endIter

      ------------------------------------------------------------------------

      step = do
        modifyIORef state (\s -> case s of
                                   Nothing -> Nothing
                                   Just (m, b) -> Just (updateMachine m, b))
        updateDebugPanel
        displayCurrentInstruction

      ------------------------------------------------------------------------

      run = do
        handlerId <- idleAdd (replicateM_ 100 step >> return True) priorityDefaultIdle
        modifyIORef state (\s -> case s of
                                   Nothing -> Nothing
                                   Just (m, _) -> Just (m, Just handlerId))
        updateRunCommandsSensitivity

      ------------------------------------------------------------------------

      pause = do
        s <- readIORef state
        case s of
          Nothing -> return ()
          Just (_, Nothing) -> return ()
          Just (_, Just handlerId) -> idleRemove handlerId
        modifyIORef state (\s -> case s of
                                   Nothing -> Nothing
                                   Just (m, _) -> Just (m, Nothing))
        updateRunCommandsSensitivity

      ------------------------------------------------------------------------

      open = do
        fileSelect <- fileChooserDialogNew
                        (Just "Open Game Boy ROM")
                        (Just window_main)
                        FileChooserActionOpen
                        [("gtk-open", ResponseOk), ("gtk-cancel", ResponseDeleteEvent)]
        response <- dialogRun fileSelect
        case response of
          ResponseOk -> do
            romFile <- fileChooserGetFilename fileSelect
            romImage <- loadRomImage (fromJust romFile)
            writeIORef state $ Just (((initialRegisterStates, initMemory romImage),
                                      initialIrqStates),
                                     Nothing)
          ResponseDeleteEvent -> do
            return ()
        widgetDestroy fileSelect
        updateRunCommandsSensitivity
        updateDebugPanel
        clearInstructionDisplay
        displayCurrentInstruction

      ------------------------------------------------------------------------

      quit = widgetDestroy window_main >> mainQuit

      ------------------------------------------------------------------------

      getMapViewerSelection :: IO Int
      getMapViewerSelection = comboBoxGetActive map_selector >>= return . fromJust

      ------------------------------------------------------------------------

      refreshMapViewer = do
        s <- readIORef state
        case s of
          Nothing -> return ()
          Just (((_, mem), _), _) -> do
            pbData <- (pixbufGetPixels mapPixBuf :: IO (PixbufData Int Word8))
            row <- pixbufGetRowstride mapPixBuf
            chan <- pixbufGetNChannels mapPixBuf
            bits <- pixbufGetBitsPerSample mapPixBuf
          
            -- draw into the Pixbuf

            mvs <- getMapViewerSelection
            case mvs of
              0 -> do
                doFromTo 0 63 $ \y ->
                  doFromTo 0 255 $ \x -> do
                    let yrow = y `div` 8
                        xrow = x `div` 8
                        tileNum = yrow * 32 + xrow
                        tileStartMem = 0x8000 + (16 * tileNum)
                        xoff = 7 - (x `mod` 8)
                        yoff = y `mod` 8
                        hiByte = tileStartMem + (yoff * 2)
                        loByte = tileStartMem + (yoff * 2) + 1
                        hiByteValue = readMem mem (fromIntegral hiByte)
                        loByteValue = readMem mem (fromIntegral loByte)
                        color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
                        colorByte = (fromIntegral color) * 85
                    writeArray pbData (x*chan+y*row) colorByte
                    writeArray pbData (1+x*chan+y*row) colorByte
                    writeArray pbData (2+x*chan+y*row) colorByte
    
                doFromTo 64 127 $ \y ->
                  doFromTo 0 255 $ \x -> do
                    let yrow = (y-64) `div` 8
                        xrow = x `div` 8
                        tileNum = yrow * 32 + xrow
                        tileStartMem = 0x8F00 + (16 * tileNum)
                        xoff = 7 - (x `mod` 8)
                        yoff = (y-64) `mod` 8
                        hiByte = tileStartMem + (yoff * 2)
                        loByte = tileStartMem + (yoff * 2) + 1
                        hiByteValue = readMem mem (fromIntegral hiByte)
                        loByteValue = readMem mem (fromIntegral loByte)
                        color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
                        colorByte = (fromIntegral color) * 85
                    writeArray pbData (x*chan+y*row) colorByte
                    writeArray pbData (1+x*chan+y*row) colorByte
                    writeArray pbData (2+x*chan+y*row) colorByte
              1 -> do
                doFromTo 0 255 $ \y ->
                  doFromTo 0 255 $ \x -> do
                    let yrow = y `div` 8
                        xrow = x `div` 8
                        tileNum = yrow * 32 + xrow
                        tileIndex = readMem mem ((fromIntegral tileNum) + 0x9800)
                        tileStartMem = 0x8000 + (16 * (fromIntegral tileIndex))
                        xoff = 7 - (x `mod` 8)
                        yoff = y `mod` 8
                        hiByte = tileStartMem + (yoff * 2)
                        loByte = tileStartMem + (yoff * 2) + 1
                        hiByteValue = readMem mem (fromIntegral hiByte)
                        loByteValue = readMem mem (fromIntegral loByte)
                        color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
                        colorByte = (fromIntegral color) * 85
                    writeArray pbData (x*chan+y*row) colorByte
                    writeArray pbData (1+x*chan+y*row) colorByte
                    writeArray pbData (2+x*chan+y*row) colorByte
              2 -> do
                doFromTo 0 255 $ \y ->
                  doFromTo 0 255 $ \x -> do
                    let yrow = y `div` 8
                        xrow = x `div` 8
                        tileNum = yrow * 32 + xrow
                        tileIndex = (fromIntegral (readMem mem ((fromIntegral tileNum) + 0x9800)))::Int8
                        tileStartMem = 0x9000 + (16 * (fromIntegral tileIndex))
                        xoff = 7 - (x `mod` 8)
                        yoff = y `mod` 8
                        hiByte = tileStartMem + (yoff * 2)
                        loByte = tileStartMem + (yoff * 2) + 1
                        hiByteValue = readMem mem (fromIntegral hiByte)
                        loByteValue = readMem mem (fromIntegral loByte)
                        color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
                        colorByte = (fromIntegral color) * 85
                    writeArray pbData (x*chan+y*row) colorByte
                    writeArray pbData (1+x*chan+y*row) colorByte
                    writeArray pbData (2+x*chan+y*row) colorByte

    
            widgetQueueDraw map_drawingarea

      ------------------------------------------------------------------------

  comboBoxSetActive map_selector 0

  menu_quit `onActivateLeaf` quit
  window_main `onDestroy` quit

  menu_open `onActivateLeaf` open
  button_open `onToolButtonClicked` open

  menu_step `onActivateLeaf` step
  button_step `onToolButtonClicked` step

  menu_run `onActivateLeaf` run
  button_run `onToolButtonClicked` run

  menu_pause `onActivateLeaf` pause
  button_pause `onToolButtonClicked` pause

  menu_about `onActivateLeaf` do
    dia <- aboutDialogNew
    aboutDialogSetName dia "OmegaGB test01"
    aboutDialogSetComments dia "Game Boy Emulator Development Test"
    aboutDialogSetWebsite dia "http://www.mutantlemon.com/omegagb"
    dialogRun dia
    widgetDestroy dia

  map_drawingarea `onSizeRequest` return (Requisition 256 256)
  map_drawingarea `onExpose` updateCanvas map_drawingarea mapPixBuf

  map_refresh `onClicked` refreshMapViewer
  main_notebook `onSwitchPage` (\pageNum -> when (pageNum == 1) refreshMapViewer)
  map_selector `onChanged` refreshMapViewer

  updateRunCommandsSensitivity

--  C.catchJust C.errorCalls
--              mainGUI
--              (\e -> do
--                 dia <- dialogNew
--                 windowSetTitle dia "Error"
--                 dialogAddButton dia "gtk-ok" ResponseOk
--                 upper <- dialogGetUpper dia
--                 message <- labelNew (Just ("Error: " ++ (show e)))
--                 widgetShow message
--                 boxPackStartDefaults upper message
--                 dialogRun dia
--                 widgetDestroy dia)

  mainGUI
  return ()

  where
    gladeFile = "guis/test01/test01.glade"
    instructionHistoryCount = 20

