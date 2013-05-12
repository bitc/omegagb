-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Machine
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module does emulation of a lot of the Game Boy's hardware, including
-- interrupts and graphics rendering. It's still incomplete.
-- 
-- The complete state of all of the GameBoy is represented as the tuple:
-- ((RegisterStates, Memory), IrqStates)
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Machine where
--import Prerequisites

import Data.Array.IArray
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.List
import Control.Monad.State

import WordUtil
import Cpu
import CpuExecution
import Memory
import Display
import Joypad

import Debug.Trace

--cpuClockFrequency :: Double
--cpuClockFrequency = 4194304

--horizSync :: Double
--horizSync = 9198000

--vertSync :: Double
--vertSync = 59.73

--vblankPeriod = cpuClockFrequency / vertSync

--scanlinePeriod = vblankPeriod / 153

-- Strict field used to contain states
data RegisterStates = RegisterStates
       {-# UNPACK #-} !Word8  -- A
       {-# UNPACK #-} !Word8  -- B
       {-# UNPACK #-} !Word8  -- C
       {-# UNPACK #-} !Word8  -- D
       {-# UNPACK #-} !Word8  -- E
       {-# UNPACK #-} !Word8  -- F
       {-# UNPACK #-} !Word8  -- H
       {-# UNPACK #-} !Word8  -- L
       {-# UNPACK #-} !Word16 -- PC
       {-# UNPACK #-} !Word16 -- SP

getRegState :: RegisterStates -> M_Register -> Word8
getRegState rs r =
  let RegisterStates a b c d e f h l _ _ = rs in
  case r of
    M_A -> a
    M_B -> b
    M_C -> c
    M_D -> d
    M_E -> e
    M_F -> f
    M_H -> h
    M_L -> l

setRegState :: RegisterStates -> M_Register -> Word8 -> RegisterStates
setRegState rs r n =
  let RegisterStates a b c d e f h l pc sp = rs in
  case r of
    M_A -> RegisterStates n b c d e f h l pc sp
    M_B -> RegisterStates a n c d e f h l pc sp
    M_C -> RegisterStates a b n d e f h l pc sp
    M_D -> RegisterStates a b c n e f h l pc sp
    M_E -> RegisterStates a b c d n f h l pc sp
    M_F -> RegisterStates a b c d e (n.&.0xF0) h l pc sp
    M_H -> RegisterStates a b c d e f n l pc sp
    M_L -> RegisterStates a b c d e f h n pc sp

getReg2State :: RegisterStates -> M_Register2 -> Word16
getReg2State rs r2 =
  let RegisterStates a b c d e f h l pc sp = rs in
  case r2 of
    M_AF -> joinWord16 a f
    M_BC -> joinWord16 b c
    M_DE -> joinWord16 d e
    M_HL -> joinWord16 h l
    M_PC -> pc
    M_SP -> sp

setReg2State :: RegisterStates -> M_Register2 -> Word16 -> RegisterStates
setReg2State rs r2 nn =
  let RegisterStates a b c d e f h l pc sp = rs
      (hi, lo) = splitWord16 nn in
  case r2 of
    M_AF -> RegisterStates hi b c d e (lo.&.0xF0) h l pc sp
    M_BC -> RegisterStates a hi lo d e f h l pc sp
    M_DE -> RegisterStates a b c hi lo f h l pc sp
    M_HL -> RegisterStates a b c d e f hi lo pc sp
    M_PC -> RegisterStates a b c d e f h l nn sp
    M_SP -> RegisterStates a b c d e f h l pc nn

initialA_GB, initialA_GBP, initialA_GBC :: Word8
initialA_GB  = 0x01
initialA_GBP = 0xFF
initialA_GBC = 0x11

initialRegisterStates :: RegisterStates
initialRegisterStates = RegisterStates
  initialA_GB  -- A
  0x00         -- B
  0x13         -- C
  0x00         -- D
  0xD8         -- E
  0xB0         -- F
  0x01         -- H
  0x4D         -- L
  0x0100       -- PC
  0xFFFE       -- SP

vBlankPeriod = 70224
hBlankPeriod = 456

divPeriod = 256

data IrqStates = IrqStates {
  irqStateIME :: Bool,               -- Interrupt Master Enable
  irqStateVBlankCounter :: Int,      -- CPU cycles until next V-Blank (mode1)
  irqStateHBlankCounter :: Int,      -- CPU cycles until next H-Blank (mode2)
  irqStateHBlankMode3Counter :: Int, -- CPU cycles until next H-Blank mode3 cycle
  irqStateHBlankMode0Counter :: Int, -- CPU cycles until next H-Blank mode0 cycle
  irqStateCurrentScanline :: Int,    -- Current scanline, 0-153
  irqStateDisplay :: Display,        -- LCD Display pixels
  irqStateVBlankNow :: Bool,         -- VBlank happened right now at the current instruction
  irqStateDIVCounter :: Int          -- DIV register
}

initialIrqStates = IrqStates {
  irqStateIME = False,
  irqStateVBlankCounter = 0,
  irqStateHBlankCounter = 0,
  irqStateHBlankMode3Counter = 80,
  irqStateHBlankMode0Counter = (80 + 172),
  irqStateCurrentScanline = 153,
  irqStateDisplay = blankDisplay,
  irqStateVBlankNow = False,
  irqStateDIVCounter = 0
}

irqUpdate :: CycleCount ->
             Maybe Bool ->
             ((RegisterStates, Memory), IrqStates) ->
             ((RegisterStates, Memory), IrqStates)
irqUpdate cycles ime = execState $ do
  when (isJust ime) (modify $ transformIrq (\i -> i { irqStateIME = fromJust ime }))
  let updateCounters i = i { irqStateVBlankCounter = (irqStateVBlankCounter i) - cycles,
                             irqStateHBlankCounter = (irqStateHBlankCounter i) - cycles,
                             irqStateHBlankMode3Counter = (irqStateHBlankMode3Counter i) - cycles,
                             irqStateHBlankMode0Counter = (irqStateHBlankMode0Counter i) - cycles,
                             irqStateDIVCounter = (irqStateDIVCounter i) - cycles }
  modify $ transformIrq updateCounters

  modify $ transformIrq (\i -> i { irqStateVBlankNow = (irqStateVBlankCounter i) <= 0 })

  (_, irq) <- get
  when ((irqStateHBlankCounter irq) <= 0)
       (do modify $ transformIrq (\i -> i { irqStateHBlankCounter = (irqStateHBlankCounter i) + hBlankPeriod })
           modify tickHBlank)
  when ((irqStateHBlankMode3Counter irq) <= 0)
       (do modify $ transformIrq (\i -> i { irqStateHBlankMode3Counter = (irqStateHBlankMode3Counter i) + hBlankPeriod })
           modify tickHBlankMode3)
  when ((irqStateHBlankMode0Counter irq) <= 0)
       (do modify $ transformIrq (\i -> i { irqStateHBlankMode0Counter = (irqStateHBlankMode0Counter i) + hBlankPeriod })
           modify tickHBlankMode0)
  when ((irqStateVBlankCounter irq) <= 0)
       (do modify $ transformIrq (\i -> i { irqStateVBlankCounter = (irqStateVBlankCounter i) + vBlankPeriod })
           modify tickVBlank)
  when ((irqStateDIVCounter irq) <= 0)
       (do modify $ transformIrq (\i -> i { irqStateDIVCounter = (irqStateDIVCounter i) + divPeriod })
           modify tickDIV)

  ((_, mem), irq2) <- get
  let ime = irqStateIME irq2
      flagsIF = (memRam mem)!0xFF0F
      flagsIE = (memRam mem)!0xFFFF
  when (ime && (flagsIF .&. flagsIE > 0))
       (let i = getLowBit (flagsIF .&. flagsIE)
            jumpAddr = case i of
                         0 -> 0x0040
                         1 -> 0x0048
                         2 -> 0x0050
                         3 -> 0x0058
                         4 -> 0x0060 in
        do modify $ transformIrq (\i -> i { irqStateIME = False })
           modify $ transformMem (transformMemoryAddr (`clearBit` i) 0xFF0F)
           ((reg, mem), _) <- get
           let oldPC = getReg2State reg M_PC
               (hiPC, loPC) = splitWord16 oldPC
               oldSP = getReg2State reg M_SP
           (flip const) (showHex2 oldSP) (modify $ transformMem ( \m -> writeMem (writeMem m (oldSP-1) hiPC) (oldSP-2) loPC ))
           modify $ transformReg (\r -> setReg2State r M_SP $! (oldSP-2))
           modify $ transformReg (\r -> setReg2State r M_PC $! jumpAddr)
           return ())

  where getLowBit :: Word8 -> Int
        getLowBit n = fromJust (elemIndex True (map (testBit n) [0..4]))

transformReg :: (RegisterStates -> RegisterStates) ->
                ((RegisterStates, Memory), IrqStates) -> 
                ((RegisterStates, Memory), IrqStates)
transformReg t ((r, m), i) = ((t r, m), i)

transformMem :: (Memory -> Memory) ->
                ((RegisterStates, Memory), IrqStates) ->
                ((RegisterStates, Memory), IrqStates)
transformMem t ((r, m), i) = ((r, t m), i)

transformIrq :: (IrqStates -> IrqStates) ->
                ((RegisterStates, Memory), IrqStates) ->
                ((RegisterStates, Memory), IrqStates)
transformIrq t ((r, m), i) = ((r, m), t i)

transformMemoryAddr :: (Word8 -> Word8) -> Word16 -> Memory -> Memory
transformMemoryAddr t a m = m { memRam = (memRam m)//[(a, (t ((memRam m)!a)))] }

tickHBlank :: ((RegisterStates, Memory), IrqStates) -> ((RegisterStates, Memory), IrqStates)
tickHBlank = execState $ do
  let incrementLY = transformMemoryAddr ((`mod` 154).(+1)) 0xFF44
  modify $ transformMem incrementLY
  modify $ transformIrq (\i -> i { irqStateCurrentScanline = (((`mod` 154).(+1)) (irqStateCurrentScanline i)) } )
  ((_, _), irq) <- get
  when ((irqStateCurrentScanline irq) < 144) $ do
    ((_, mem), _) <- get
    let (ly, lyc) = ((memRam mem)!0xFF44, (memRam mem)!0xFF45)
        (statB5, statB6) = ((memRam mem)!0xFF41 `testBit` 5, (memRam mem)!0xFF41 `testBit` 6)
        updateSTATFlags = transformMemoryAddr
                            (execState $ do
                               modify (`clearBit` 0)
                               modify (`setBit` 1)
                               modify (if ly==lyc then (`setBit` 2) else (`clearBit` 2)))
                            (0xFF41)
        updateIF = transformMemoryAddr
                     (execState $ when (statB5 || (statB6 && ly==lyc)) (modify (`setBit` 1)))
                     (0xFF0F)
    modify $ transformMem updateSTATFlags
    modify $ transformMem updateIF
    modify $ renderScanLine (irqStateCurrentScanline irq)

tickHBlankMode3 :: ((RegisterStates, Memory), IrqStates) -> ((RegisterStates, Memory), IrqStates)
tickHBlankMode3 = execState $ do
  ((_, _), irq) <- get
  when ((irqStateCurrentScanline irq) < 144) $ do
    let updateSTATFlags = transformMemoryAddr
                            (execState $ do
                               modify (`setBit` 0)
                               modify (`setBit` 1))
                            (0xFF41)
    modify $ transformMem updateSTATFlags

tickHBlankMode0 :: ((RegisterStates, Memory), IrqStates) -> ((RegisterStates, Memory), IrqStates)
tickHBlankMode0 = execState $ do
  ((_, _), irq) <- get
  when ((irqStateCurrentScanline irq) < 144) $ do
    ((_, mem), _) <- get
    let statB3 = (memRam mem)!0xFF41 `testBit` 3
        updateSTATFlags = transformMemoryAddr
                            (execState $ do
                               modify (`clearBit` 0)
                               modify (`clearBit` 1))
                            (0xFF41)
        updateIF = transformMemoryAddr
                     (execState $ when statB3 (modify (`setBit` 1)))
                     (0xFF0F)
    modify $ transformMem updateSTATFlags
    modify $ transformMem updateIF

tickVBlank :: ((RegisterStates, Memory), IrqStates) -> ((RegisterStates, Memory), IrqStates)
tickVBlank = execState $ do
  ((_, mem), _) <- get
  let statB4 = (memRam mem)!0xFF41 `testBit` 4
      updateSTATFlags = transformMemoryAddr
                          (execState $ do
                             modify (`setBit` 0)
                             modify (`clearBit` 1))
                          (0xFF41)
      updateIF = transformMemoryAddr
                   (execState $ do
                      when statB4 (modify (`setBit` 1))
                      modify (`setBit` 0))
                   (0xFF0F)
  modify $ transformMem updateSTATFlags
  modify $ transformMem updateIF

tickDIV :: ((RegisterStates, Memory), IrqStates) -> ((RegisterStates, Memory), IrqStates)
tickDIV = execState $ do
  ((_, mem), _) <- get
  modify $ transformMem (transformMemoryAddr (+1) 0xFF04)

renderScanLine :: Int -> ((RegisterStates, Memory), IrqStates) -> ((RegisterStates, Memory), IrqStates)
renderScanLine scanline = execState $ do
  ((_, mem), irq) <- get
  let d = irqStateDisplay irq
  let scx = fromIntegral (readMem mem 0xFF43)
  let scy = fromIntegral (readMem mem 0xFF42)
  let lcdc = readMem mem 0xFF40
  let lcdon = testBit lcdc 7
  let bgon = testBit lcdc 0
  let bgmap = testBit lcdc 3
  let bgmapStartAddr = if bgmap then 0x9C00 else 0x9800
  let bgtiles = testBit lcdc 4
  let bgtilesStartAddr = if bgtiles then 0x8000 else 0x9000
  let spritesOn = testBit lcdc 1
  let spritesBig = testBit lcdc 2
  let getBgPixel :: Int -> Int -> Word8
      getBgPixel x y =
        let x' = (x + scx) `mod` 256
            y' = (y + scy) `mod` 256
            yrow = y' `div` 8
            xrow = x' `div` 8
            tileNum = yrow * 32 + xrow
            tileIndex = (fromIntegral (readMem mem ((fromIntegral tileNum) + bgmapStartAddr)))::Int8
            tileStartMem = bgtilesStartAddr + (16 * fromIntegral tileIndex)
            xoff = 7 - (x' `mod` 8)
            yoff = y' `mod` 8
            hiByte = tileStartMem + (yoff * 2)
            loByte = tileStartMem + (yoff * 2) + 1
            hiByteValue = readMem mem (fromIntegral hiByte)
            loByteValue = readMem mem (fromIntegral loByte)
            color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
        in if bgon && lcdon then fromIntegral color else 0
  let getSpritesPixel :: Int -> Int -> Maybe Word8
      getSpritesPixel x y = case spritesBig of
        False -> let xposes = map ( \i -> readMem mem (0xFE00 + (i*4) + 1) ) [0..39]
                     yposes = map ( \i -> readMem mem (0xFE00 + (i*4) + 0) ) [0..39]
                     tileIndexes = map ( \i -> readMem mem (0xFE00 + (i*4) + 2) ) [0..39]
                     xflips = map ( \i -> testBit (readMem mem (0xFE00 + (i*4) + 2)) 5 ) [0..39]
                     yflips = map ( \i -> testBit (readMem mem (0xFE00 + (i*4) + 2)) 4 ) [0..39]
                     getSingleSpritePixel :: Int -> Int -> Int -> Int -> Word8 -> Bool -> Bool -> Maybe Word8
                     getSingleSpritePixel x y xpos ypos tileIndex xflip yflip =
                       if x > (fromIntegral xpos)-8 && x <= xpos &&
                          y > (fromIntegral ypos)-16 && y <= ypos-8
                       then let x' = x - (fromIntegral xpos) + 8
                                y' = y - (fromIntegral ypos) + 16
                                x'' = if xflip then 7-x' else x'
                                y'' = if yflip then 7-y' else y'
                                tileStartMem = 0x8000 + (16 * (fromIntegral tileIndex))
                                xoff = 7 - (x'' `mod` 8)
                                yoff = y'' `mod` 8
                                hiByte = tileStartMem + (yoff * 2)
                                loByte = tileStartMem + (yoff * 2) + 1
                                hiByteValue = readMem mem (fromIntegral hiByte)
                                loByteValue = readMem mem (fromIntegral loByte)
                                color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
                            in if color > 0 then Just (fromIntegral color) else Nothing
                       else Nothing
                 in foldl' ( \a b -> if isJust a
                                     then a
                                     else getSingleSpritePixel x
                                                               y
                                                               (fromIntegral (xposes!!b))
                                                               (fromIntegral (yposes!!b))
                                                               (tileIndexes!!b)
                                                               (xflips!!b)
                                                               (yflips!!b)
                           ) Nothing [0..39]
        True -> trace ("eek") Nothing

  let getPixel x y = case getSpritesPixel x y of
                       Just c -> c
                       Nothing -> getBgPixel x y

  let updateList = map (\x -> ((scanline, x), getBgPixel x scanline)) [0..159]
  let d' = d//updateList
  modify $ transformIrq (\i -> i { irqStateDisplay = d' })


machineCpuExecute :: (MemoryModel m) =>
                     (RegisterStates, m) ->
                     ExecutionAST () ->
                     (RegisterStates, m)
machineCpuExecute s e = fst (machineCpuExecute' s e)

machineCpuExecute' :: (MemoryModel m) =>
                      (RegisterStates, m) ->
                      ExecutionAST a ->
                      ((RegisterStates, m), a)
machineCpuExecute' state@(regS, memS) e = case e of
  Return result -> (state, result)
  Bind l r -> let (s, result) = machineCpuExecute' state l in
              machineCpuExecute' s (r result)
  WriteRegister reg n -> ((setRegState regS reg n, memS), ())
  ReadRegister reg -> (state, getRegState regS reg)
  WriteRegister2 reg2 nn -> ((setReg2State regS reg2 nn, memS), ())
  ReadRegister2 reg2 -> (state, getReg2State regS reg2)
  WriteMemory a n -> ((regS, writeMem memS a n), ())
  ReadMemory a -> (state, readMem memS a)

fetchInstruction :: (MemoryModel m) => (RegisterStates, m) -> Instruction
fetchInstruction (regS, memS) =
  let pc = getReg2State regS M_PC
      opcode = readMem memS pc
      n :: Word8
      n = readMem memS (pc+1)
      nn :: Word16
      nn = joinWord16 (readMem memS (pc+2)) (readMem memS (pc+1))
      instruction = machineCodeToInstruction opcode (n, nn) in
  instruction

machineStepInstruction :: (MemoryModel m) =>
                          (RegisterStates, m) ->
                          (RegisterStates, m)
machineStepInstruction state@(regS, memS) =
  let instruction = fetchInstruction state
      execution = executeInstruction instruction in
  machineCpuExecute state execution

updateMachine :: ((RegisterStates, Memory), IrqStates) ->
                 ((RegisterStates, Memory), IrqStates)
updateMachine (state@(regS, memS), irqS) =
  let stepInstruction = machineStepInstruction state
      pc = getReg2State regS M_PC
      opcode = readMem memS pc
      cycles = opcodeCycleCount opcode
      ime = opcodeQueryIME opcode in
  irqUpdate cycles ime (stepInstruction, irqS)

initialMachineState romImage =
  ((initialRegisterStates, initMemory romImage), initialIrqStates)

--instance Show JoypadKeyStates

updateMachineDisplayFrame :: JoypadKeyStates ->
                             ((RegisterStates, Memory), IrqStates) ->
                             (Display, ((RegisterStates, Memory), IrqStates))
updateMachineDisplayFrame jp s =
  let s' = transformMem (\mem -> mem { memJoypadKeyStates = jp } ) s
      l = tail (iterate updateMachine s')
      pred (_, irq) = irqStateVBlankNow irq
      f@(_, irqS) = fromJust (find pred l) in
  (irqStateDisplay irqS, f)

--updateMachineDisplayFrame' jp s =
--  let (_, s') = updateMachineDisplayFrame jp s in
--  updateMachineDisplayFrame jp s'

