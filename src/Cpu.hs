-- OmegaGB Copyright 2007 Bit Connor
-- This program is distributed under the terms of the GNU General Public License
-----------------------------------------------------------------------------
-- |
-- Module      :  Cpu
-- Copyright   :  (c) Bit Connor 2007 <bit@mutantlemon.com>
-- License     :  GPL
-- Maintainer  :  bit@mutantlemon.com
-- Stability   :  in-progress
--
-- OmegaGB
-- Game Boy Emulator
--
-- This module does emulation of the Game Boy's Z80 like CPU.
-- A few instructions still need to be implemented
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Cpu where

import Data.Word
import Data.Int
import Data.Bits

import CpuExecution
import WordUtil

int8FromWord8 :: Word8 -> Int8
int8FromWord8 = fromIntegral

swapNibbles :: Word8 -> Word8
swapNibbles n = rotate n 4

type Opcode = Word8
type CycleCount = Int
type ArgumentCount = Int

data Register =
  A |
  B |
  C |
  D |
  E |
  H |
  L

data RegisterPair =
  BC |
  DE |
  HL |
  SP

data StackRegister =
  StackRegAF |
  StackRegBC |
  StackRegDE |
  StackRegHL

data RestartAddress =
  RST_00H |
  RST_08H |
  RST_10H |
  RST_18H |
  RST_20H |
  RST_28H |
  RST_30H |
  RST_38H

data BitIndex =
  Bit0 |
  Bit1 |
  Bit2 |
  Bit3 |
  Bit4 |
  Bit5 |
  Bit6 |
  Bit7

data FlagCondition =
  FlagC |
  FlagNC |
  FlagNZ |
  FlagZ

data Instruction =
  LDR Register Register             | -- LD r,r
  LDRN Register Word8               | -- LD r,n
  LDRHL Register                    | -- LD r,(HL)
  LDHL Register                     | -- LD (HL),r
  LDHLN Word8                       | -- LD (HL),n
  LDAP Bool                         | -- LD A,(BC)           LD A,(DE)
  LDAPN Word16                      | -- LD A,(nn)
  LDAC                              | -- LD A,($FF00+C)
  LDPR Bool                         | -- LD (BC),A           LD (DE),A
  LDPN Word16                       | -- LD (nn),A
  LDPC                              | -- LD ($FF00+C),A
  LD2 RegisterPair Word16           | -- LD rp,nn
  LDSP2                             | -- LD SP,HL
  LDP2 Word16                       | -- LD (nn),SP
  LDI Bool                          | -- LD (HL),A           LD A,(HL)
  LDD Bool                          | -- LD (HL),A           LD A,(HL)
  LDH Bool Word8                    | -- LD ($FF00+n),A      LD A,($FF00+n)
  LDHL2 Int8                        | -- LD HL,(SP+n)
  PUSH StackRegister                | -- PUSH sr
  POP StackRegister                 | -- POP sr
  ADD Register                      | -- ADD A,r
  ADDN Word8                        | -- ADD A,n
  ADDHL                             | -- ADD A,(HL)
  ADC Register                      | -- ADC A,r
  ADCN Word8                        | -- ADC A,n
  ADCHL                             | -- ADC A,(HL)
  SUB Register                      | -- SUB A,r
  SUBN Word8                        | -- SUB A,n
  SUBHL                             | -- SUB A,(HL)
  SBC Register                      | -- SBC A,r
  SBCN Word8                        | -- SBC A,n
  SBCHL                             | -- SBC A,(HL)
  AND Register                      | -- AND A,r
  ANDN Word8                        | -- AND A,n
  ANDHL                             | -- AND A,(HL)
  OR Register                       | -- OR A,r
  ORN Word8                         | -- OR A,n
  ORHL                              | -- OR A,(HL)
  XOR Register                      | -- XOR A,r
  XORN Word8                        | -- XOR A,n
  XORHL                             | -- XOR A,(HL)
  CP Register                       | -- CP A,r
  CPN Word8                         | -- CP A,n
  CPHL                              | -- CP A,(HL)
  INC Register                      | -- INC r
  INC2 RegisterPair                 | -- INC rp
  INCHL                             | -- INC (HL)
  DEC Register                      | -- DEC r
  DEC2 RegisterPair                 | -- DEC rp
  DECHL                             | -- DEC (HL)
  ADD2HL RegisterPair               | -- ADD HL,rp
  ADD2SP Int8                       | -- ADD SP,o
  SWAP Register                     | -- SWAP r
  SWAPHL                            | -- SWAP (HL)
  DAA                               | -- DAA
  CPL                               | -- CPL
  CCF                               | -- CCF
  SCF                               | -- SCF
  NOP                               | -- NOP
  HALT                              | -- * HALT
  STOP                              | -- * STOP
  DI                                | -- * DI
  EI                                | -- * EI
  RLCA                              | -- RLCA
  RLA                               | -- RLA
  RRCA                              | -- RRCA
  RRA                               | -- RRA
  RLC Register                      | -- RLC r
  RLCHL                             | -- RLC (HL)
  RL Register                       | -- RL r
  RLHL                              | -- RL (HL)
  RRC Register                      | -- RRC r
  RRCHL                             | -- RRC (HL)
  RR Register                       | -- RR r
  RRHL                              | -- RR (HL)
  SLA Register                      | -- SLA r
  SLAHL                             | -- SLA (HL)
  SRA Register                      | -- SRA r
  SRAHL                             | -- SRA (HL)
  SRL Register                      | -- SRL r
  SRLHL                             | -- SRL (HL)
  BIT BitIndex Register             | -- BIT b,r
  BITHL BitIndex                    | -- BIT b,(HL)
  SET BitIndex Register             | -- SET b,r
  SETHL BitIndex                    | -- SET b,(HL)
  RES BitIndex Register             | -- RES b,r
  RESHL BitIndex                    | -- RES b,(HL)
  JP (Maybe FlagCondition) Word16   | -- JP nn               JP cc,nn
  JPHL                              | -- JP (HL)
  JR (Maybe FlagCondition) Int8     | -- JR a                JR cc,a
  CALL (Maybe FlagCondition) Word16 | -- CALL a              CALL cc,a
  RST RestartAddress                | -- RST n
  RET (Maybe FlagCondition)         | -- RET                 RET cc
  RETI                                -- * RETI

interruptQueryInstruction :: Instruction -> Maybe Bool
interruptQueryInstruction ins = case ins of
  DI -> Just False
  EI -> Just True
  RETI -> Just True
  _ -> Nothing

opcodeQueryIME :: Opcode -> Maybe Bool
opcodeQueryIME 0xF3 = Just False
opcodeQueryIME 0xFB = Just True
opcodeQueryIME 0xD9 = Just True
opcodeQueryIME _ = Nothing

mRegister :: Register -> M_Register
mRegister r = case r of
  A -> M_A
  B -> M_B
  C -> M_C
  D -> M_D
  E -> M_E
  H -> M_H
  L -> M_L
  
mRegister2 :: RegisterPair -> M_Register2
mRegister2 rp = case rp of
  BC -> M_BC
  DE -> M_DE
  HL -> M_HL
  SP -> M_SP

mRegisterS :: StackRegister -> M_Register2
mRegisterS sr = case sr of
  StackRegAF -> M_AF
  StackRegBC -> M_BC
  StackRegDE -> M_DE
  StackRegHL -> M_HL

executeInstruction :: Instruction -> ExecutionAST ()
executeInstruction ins = case ins of
  LDR r1 r2 -> do
    a <- readRegister (mRegister r2)
    writeRegister (mRegister r1) a
    incPC1
  LDRN r n -> do
    writeRegister (mRegister r) n
    incPC2
  LDRHL r -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    writeRegister (mRegister r) v
    incPC1
  LDHL r -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister r)
    writeMemory a v
    incPC1
  LDHLN n -> do
    a <- readRegister2 (mRegister2 HL)
    writeMemory a n
    incPC2
  LDAP True -> do
    a <- readRegister2 (mRegister2 BC)
    v <- readMemory a
    writeRegister (mRegister A) v
    incPC1
  LDAP False -> do
    a <- readRegister2 (mRegister2 DE)
    v <- readMemory a
    writeRegister (mRegister A) v
    incPC1
  LDAPN nn -> do
    v <- readMemory nn
    writeRegister (mRegister A) v
    incPC3
  LDAC -> do
    o <- readRegister (mRegister C)
    let a = 0xFF00 + (fromIntegral o)
    v <- readMemory a
    writeRegister (mRegister A) v
    incPC1
  LDPR True -> do
    a <- readRegister2 (mRegister2 BC)
    v <- readRegister (mRegister A)
    writeMemory a v
    incPC1
  LDPR False -> do
    a <- readRegister2 (mRegister2 DE)
    v <- readRegister (mRegister A)
    writeMemory a v
    incPC1
  LDPN a -> do
    v <- readRegister (mRegister A)
    writeMemory a v
    incPC3
  LDPC -> do
    o <- readRegister (mRegister C)
    let a = 0xFF00 + (fromIntegral o)::Word16
    v <- readRegister (mRegister A)
    writeMemory a v
    incPC1
  LD2 rp nn -> do
    writeRegister2 (mRegister2 rp) nn
    incPC3
  LDSP2 -> do
    v <- readRegister2 (mRegister2 HL)
    writeRegister2 (mRegister2 SP) v
    incPC1
  LDP2 nn -> do
    v <- readRegister2 (mRegister2 SP)
    let (v', v'') = splitWord16 v
    let nn' = nn + 1
    writeMemory nn v'
    writeMemory nn' v''
    incPC3
  LDI True -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister A)
    writeMemory a v
    let a' = a + 1
    writeRegister2 (mRegister2 HL) a'
    incPC1
  LDI False -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    writeRegister (mRegister A) v
    let a' = a + 1
    writeRegister2 (mRegister2 HL) a'
    incPC1
  LDD True -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister A)
    writeMemory a v
    let a' = a - 1
    writeRegister2 (mRegister2 HL) a'
    incPC1
  LDD False -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    writeRegister (mRegister A) v
    let a' = a - 1
    writeRegister2 (mRegister2 HL) a'
    incPC1
  LDH True n -> do
    let a = 0xFF00 + (fromIntegral n)
    v <- readRegister (mRegister A)
    writeMemory a v
    incPC2
  LDH False n -> do
    let a = 0xFF00 + (fromIntegral n)
    v <- readMemory a
    writeRegister (mRegister A) v
    incPC2
  LDHL2 d -> do
    v <- readRegister2 (mRegister2 SP)
    let v' = v + (fromIntegral d)
    writeRegister2 (mRegister2 HL) v'
    writeFlags (Just False) (Just False) (Just (v'.&.0x000F < v.&.0x000F)) (Just (v' < v))
    incPC2
  PUSH sr -> do
    a <- readRegister2 (mRegister2 SP)
    let a' = a - 1
    let a'' = a - 2
    v <- readRegister2 (mRegisterS sr)
    let (hi, lo) = splitWord16 v
    writeMemory a' hi
    writeMemory a'' lo
    writeRegister2 (mRegister2 SP) a''
    incPC1
  POP sr -> do
    a <- readRegister2 (mRegister2 SP)
    let a' = a + 1
    lo <- readMemory a
    hi <- readMemory a'
    let v = joinWord16 hi lo
    writeRegister2 (mRegisterS sr) v
    let a'' = a + 2
    writeRegister2 (mRegister2 SP) a''
    incPC1
  ADD r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    let v'' = v + v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just (v'' .&. 0x0F < v .&. 0x0F)) (Just (v'' < v))
    incPC1
  ADDN n -> do
    v <- readRegister (mRegister A)
    let v' = v + n
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just False) (Just (v' .&. 0x0F < v .&. 0x0F)) (Just (v' < v))
    incPC2
  ADDHL -> do
    v <- readRegister (mRegister A)
    a <- readRegister2 (mRegister2 HL)
    v' <- readMemory a
    let v'' = v + v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just (v'' .&. 0x0F < v .&. 0x0F)) (Just (v'' < v))
    incPC1
  ADC r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    f <- readRegister M_F
    let carry = if testBit f 4 then 1 else 0
    let v'' = v + v' + carry
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just (v'' .&. 0x0F < v .&. 0x0F)) (Just (v'' < v))
    incPC1
  ADCN n -> do
    v <- readRegister (mRegister A)
    f <- readRegister M_F
    let carry = if testBit f 4 then 1 else 0
    let v' = v + n + carry
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just False) (Just (v' .&. 0x0F < v .&. 0x0F)) (Just (v' < v))
    incPC2
  ADCHL -> do
    v <- readRegister (mRegister A)
    a <- readRegister2 (mRegister2 HL)
    v' <- readMemory a
    f <- readRegister M_F
    let carry = if testBit f 4 then 1 else 0
    let v'' = v + v' + carry
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just (v'' .&. 0x0F < v .&. 0x0F)) (Just (v'' < v))
    incPC1
  SUB r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    let v'' = v - v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just True) (Just (v'' .&. 0x0F > v .&. 0x0F)) (Just (v'' > v))
    incPC1
  SUBN n -> do
    v <- readRegister (mRegister A)
    let v' = v - n
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just True) (Just (v' .&. 0x0F > v .&. 0x0F)) (Just (v' > v))
    incPC2
  SUBHL -> do
    v <- readRegister (mRegister A)
    a <- readRegister2 (mRegister2 HL)
    v' <- readMemory a
    let v'' = v - v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just True) (Just (v'' .&. 0x0F > v .&. 0x0F)) (Just (v'' > v))
    incPC1
  SBC r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    f <- readRegister M_F
    let carry = if testBit f 4 then 1 else 0
    let v'' = v - v' - carry
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just True) (Just (v'' .&. 0x0F > v .&. 0x0F)) (Just (v'' > v))
    incPC1
  SBCN n -> do
    v <- readRegister (mRegister A)
    f <- readRegister M_F
    let carry = if testBit f 4 then 1 else 0
    let v' = v - n - carry
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just True) (Just (v' .&. 0x0F > v .&. 0x0F)) (Just (v' > v))
    incPC2
  SBCHL -> do
    v <- readRegister (mRegister A)
    a <- readRegister2 (mRegister2 HL)
    v' <- readMemory a
    f <- readRegister M_F
    let carry = if testBit f 4 then 1 else 0
    let v'' = v - v' - carry
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just True) (Just (v'' .&. 0x0F > v .&. 0x0F)) (Just (v'' > v))
    incPC1
  AND r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    let v'' = v .&. v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just True) (Just False)
    incPC1
  ANDN n -> do
    v <- readRegister (mRegister A)
    let v' = v .&. n
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just False) (Just True) (Just False)
    incPC2
  ANDHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister A)
    v' <- readMemory a
    let v'' = v .&. v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just True) (Just False)
    incPC1
  OR r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    let v'' = v .|. v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just False) (Just False)
    incPC1
  ORN n -> do
    v <- readRegister (mRegister A)
    let v' = v .|. n
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just False)
    incPC2
  ORHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister A)
    v' <- readMemory a
    let v'' = v .|. v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just False) (Just False)
    incPC1
  XOR r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    let v'' = v `xor` v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just False) (Just False)
    incPC1
  XORN n -> do
    v <- readRegister (mRegister A)
    let v' = v `xor` n
    writeRegister (mRegister A) v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just False)
    incPC2
  XORHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister A)
    v' <- readMemory a
    let v'' = v `xor` v'
    writeRegister (mRegister A) v''
    writeFlags (Just (v'' == 0)) (Just False) (Just False) (Just False)
    incPC1
  CP r -> do
    v <- readRegister (mRegister A)
    v' <- readRegister (mRegister r)
    writeFlags (Just (v == v')) (Just True) (Just (v .&. 0x0F < v' .&. 0x0F)) (Just (v < v'))
    incPC1
  CPN n -> do
    v <- readRegister (mRegister A)
    writeFlags (Just (v == n)) (Just True) (Just (v .&. 0x0F < n .&. 0x0F)) (Just (v < n))
    incPC2
  CPHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readRegister (mRegister A)
    v' <- readMemory a
    writeFlags (Just (v == v')) (Just True) (Just (v .&. 0x0F < v' .&. 0x0F)) (Just (v < v'))
    incPC1
  INC r -> do
    v <- readRegister (mRegister r)
    let v' = v + 1
    writeRegister (mRegister r) v'
    writeFlags (Just (v' == 0)) (Just False) (Just (v == 0x0F)) Nothing
    incPC1
  INC2 rp -> do
    v <- readRegister2 (mRegister2 rp)
    let v' = v + 1
    writeRegister2 (mRegister2 rp) v'
    incPC1
  INCHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = v + 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just (v == 0x0F)) Nothing
    incPC1
  DEC r -> do
    v <- readRegister (mRegister r)
    let v' = v - 1
    writeRegister (mRegister r) v'
    writeFlags (Just (v' == 0)) (Just True) (Just (v /= 0xF0)) Nothing
    incPC1
  DEC2 rp -> do
    v <- readRegister2 (mRegister2 rp)
    let v' = v - 1
    writeRegister2 (mRegister2 rp) v'
    incPC1
  DECHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = v - 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just True) (Just (v /= 0xF0)) Nothing
    incPC1
  ADD2HL rp -> do
    v <- readRegister2 (mRegister2 HL)
    v' <- readRegister2 (mRegister2 rp)
    let v'' = v + v'
    writeRegister2 (mRegister2 HL) v''
    -- TODO according to no$gmba only C flag is modified. is this correct?
    writeFlags Nothing Nothing Nothing (Just (v'' < v))
    incPC1
  ADD2SP n -> error "ADD SP,o | NOT IMPLEMENTED"
  SWAP r -> do
    v <- readRegister (mRegister r)
    let v' = swapNibbles v
    writeRegister (mRegister r) v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just False)
    incPC2
  SWAPHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = swapNibbles v
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just False)
    incPC2
  DAA -> error "DAA | NOT IMPLEMENTED"
  CPL -> do
    v <- readRegister (mRegister A)
    let v' = complement v
    writeRegister (mRegister A) v'
    writeFlags Nothing (Just True) (Just True) Nothing
    incPC1
  CCF -> do
    f <- readRegister M_F
    let c = testBit f 4
    writeFlags Nothing (Just False) (Just False) (Just (not c))
    incPC1
  SCF -> do
    writeFlags Nothing (Just False) (Just False) (Just True)
    incPC1
  NOP -> incPC1
  HALT -> incPC2 -- error "HALT | NOT IMPLEMENTED" >> incPC1
  STOP -> incPC2 -- error "STOP | NOT IMPLEMENTED" >> incPC2 {- TODO verify dummy arg=1 -}
  DI -> incPC1
  EI -> incPC1
  RLCA -> rlc A >> incPC1
  RLA -> rl A >> incPC1
  RRCA -> rrc A >> incPC1
  RRA -> rr A >> incPC1
  RLC r -> rlc r >> incPC2
  RLCHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = rotateL v 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 7))
    incPC2
  RL r -> rl r >> incPC2
  RLHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    f <- readRegister M_F
    let c = testBit f 4
    let v' = if c then setBit (shiftL v 1) 0 else shiftL v 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 7))
    incPC2
  RRC r -> rrc r >> incPC2
  RRCHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = rotateR v 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 0))
    incPC2
  RR r -> rr r >> incPC2
  RRHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    f <- readRegister M_F
    let c = testBit f 4
    let v' = if c then setBit (shiftR v 1) 7 else shiftR v 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 0))
    incPC2
  SLA r -> do
    v <- readRegister (mRegister r)
    let v' = shiftL v 1
    writeRegister (mRegister r) v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 7))
    incPC2
  SLAHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = shiftL v 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 7))
    incPC2
  SRA r -> error "SRA r | NOT IMPLEMENTED"
  SRAHL -> error "SRA (HL) | NOT IMPLEMENTED"
  SRL r -> do
    v <- readRegister (mRegister r)
    let v' = shiftR v 1
    writeRegister (mRegister r) v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 0))
    incPC2
  SRLHL -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = shiftR v 1
    writeMemory a v'
    writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 0))
    incPC2
  BIT bi r -> do
    v <- readRegister (mRegister r)
    let z = not (testBit v (intFromBitIndex bi))
    writeFlags (Just z) (Just False) (Just True) Nothing
    incPC2
  BITHL bi -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let z = not (testBit v (intFromBitIndex bi))
    writeFlags (Just z) (Just False) (Just True) Nothing
    incPC2
  SET bi r -> do
    v <- readRegister (mRegister r)
    let v' = setBit v (intFromBitIndex bi)
    writeRegister (mRegister r) v'
    incPC2
  SETHL bi -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = setBit v (intFromBitIndex bi)
    writeMemory a v'
    incPC2
  RES bi r -> do
    v <- readRegister (mRegister r)
    let v' = clearBit v (intFromBitIndex bi)
    writeRegister (mRegister r) v'
    incPC2
  RESHL bi -> do
    a <- readRegister2 (mRegister2 HL)
    v <- readMemory a
    let v' = clearBit v (intFromBitIndex bi)
    writeMemory a v'
    incPC2
  JP Nothing nn -> do
    writeRegister2 M_PC nn
  JP (Just cc) nn -> do
    j <- testCC cc
    if j then writeRegister2 M_PC nn else incPC3
  JPHL -> do
    a <- readRegister2 (mRegister2 HL)
    writeRegister2 M_PC a
  JR Nothing n -> do
    pc <- readRegister2 M_PC
    let a = pc + (fromIntegral n) + 2
    writeRegister2 M_PC a
  JR (Just cc) n -> do
    j <- testCC cc
    if j then executeInstruction (JR Nothing n) else incPC2
  CALL Nothing nn -> do
    pc <- readRegister2 M_PC
    a <- readRegister2 (mRegister2 SP)
    let a' = a - 1
    let a'' = a - 2
    let (hi, lo) = splitWord16 (pc+3)
    writeMemory a' hi
    writeMemory a'' lo
    writeRegister2 (mRegister2 SP) a''
    writeRegister2 M_PC nn
  CALL (Just cc) nn -> do
    j <- testCC cc
    if j then executeInstruction (CALL Nothing nn) else incPC3
  RST ra -> let ja = nFromRestartAddress ra 
                (v, v') = splitWord16 ja in do
    pc <- readRegister2 M_PC
    let (hi, lo) = splitWord16 (pc+2)
    a <- readRegister2 (mRegister2 SP)
    let a' = a - 1
    let a'' = a - 2
    writeMemory a' hi
    writeMemory a'' lo
    writeRegister2 (mRegister2 SP) a''
    writeRegister2 M_PC ja
  RET Nothing -> do
    a <- readRegister2 (mRegister2 SP)
    let a' = a + 1
    v <- readMemory a
    v' <- readMemory a'
    let v'' = joinWord16 v' v
    writeRegister2 M_PC v''
    let a'' = a + 2
    writeRegister2 (mRegister2 SP) a''
  RET (Just cc) -> do
    j <- testCC cc
    if j then executeInstruction (RET Nothing) else incPC1
  RETI -> executeInstruction (RET Nothing)
  where
    incPC1, incPC2, incPC3 :: ExecutionAST ()
    incPC1 = do
      pc <- readRegister2 M_PC
      let pc' = pc + 1
      writeRegister2 M_PC pc'
    incPC2 = do
      pc <- readRegister2 M_PC
      let pc' = pc + 2
      writeRegister2 M_PC pc'
    incPC3 = do
      pc <- readRegister2 M_PC
      let pc' = pc + 3
      writeRegister2 M_PC pc'

    testCC :: FlagCondition -> ExecutionAST Bool
    testCC FlagNZ = do
      f <- readRegister M_F
      let z = testBit f 7
      return (not z)
    testCC FlagZ = do
      f <- readRegister M_F
      let z = testBit f 7
      return z
    testCC FlagNC = do
      f <- readRegister M_F
      let c = testBit f 4
      return (not c)
    testCC FlagC = do
      f <- readRegister M_F
      let c = testBit f 4
      return c
    rlc :: Register -> ExecutionAST ()
    rlc r = do
      v <- readRegister (mRegister r)
      let v' = rotateL v 1
      writeRegister (mRegister r) v'
      writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 7))

    rl :: Register -> ExecutionAST ()
    rl r = do
      v <- readRegister (mRegister r)
      f <- readRegister M_F
      let c = testBit f 4
      let v' = if c then setBit (shiftL v 1) 0 else shiftL v 1
      writeRegister (mRegister r) v'
      writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 7))

    rrc :: Register -> ExecutionAST ()
    rrc r = do
      v <- readRegister (mRegister r)
      let v' = rotateR v 1
      writeRegister (mRegister r) v'
      writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 0))

    rr :: Register -> ExecutionAST ()
    rr r = do
      v <- readRegister (mRegister r)
      f <- readRegister M_F
      let c = testBit f 4
      let v' = if c then setBit (shiftR v 1) 7 else shiftR v 1
      writeRegister (mRegister r) v'
      writeFlags (Just (v' == 0)) (Just False) (Just False) (Just (testBit v 0))

instance Show Instruction where
  show ins = case ins of
    LDR a b -> "LD" &+ (showRegister a) ++ "," ++ (showRegister b)
    LDRN r n -> "LD" &+ (showRegister r) ++ "," ++ (show n)
    LDRHL r -> "LD" &+ (showRegister r) ++ ",(HL)"
    LDHL r -> "LD (HL)," ++ (showRegister r)
    LDHLN n -> "LD (HL)," ++ (show n)
    LDAP b -> "LD A," ++ (if b then "(BC)" else "(DE)")
    LDAPN a -> "LD A,(" ++ (showAddress a) ++ ")"
    LDAC -> "LD A,($FF00+C)"
    LDPR True -> "LD (BC),A"
    LDPR False -> "LD (DE),A"
    LDPN a -> "LD (" ++ (showAddress a) ++ "),A"
    LDPC -> "LD ($FF00+C),A"
    LD2 rp a -> "LD" &+ (showRegisterPair rp) ++ "," ++ (showAddress a)
    LDSP2 -> "LD SP,HL"
    LDP2 a -> "LD (" ++ (showAddress a) ++ "),SP"
    LDI b -> "LDI" &+ (if b then "(HL),A" else "A,(HL)")
    LDD b -> "LDD" &+ (if b then "(HL),A" else "A,(HL)")
    LDH b n -> "LDH" &+ (if b then "($FF00+"++(showWord8 n)++"),A"
                              else "A,($FF00+"++(showWord8 n)++")")
    LDHL2 o -> "LDHL SP," ++ (showOffset o)
    PUSH sr -> "PUSH" &+ (showStackRegister sr)
    POP sr -> "POP" &+ (showStackRegister sr)
    ADD r -> "ADD A," ++ (showRegister r)
    ADDN n -> "ADD A," ++ (show n)
    ADDHL -> "ADD A,(HL)"
    ADC r -> "ADC A," ++ (showRegister r)
    ADCN n -> "ADC A," ++ (show n)
    ADCHL -> "ADC A,(HL)"
    SUB r -> "SUB A," ++ (showRegister r)
    SUBN n -> "SUB A," ++ (show n)
    SUBHL -> "SUB A,(HL)"
    SBC r -> "SBC A," ++ (showRegister r)
    SBCN n -> "SBC A," ++ (show n)
    SBCHL -> "SBC A,(HL)"
    AND r -> "AND A," ++ (showRegister r)
    ANDN n -> "AND A," ++ (show n)
    ANDHL -> "AND A,(HL)"
    OR r -> "OR A," ++ (showRegister r)
    ORN n -> "OR A," ++ (show n)
    ORHL -> "OR A,(HL)"
    XOR r -> "XOR A," ++ (showRegister r)
    XORN n -> "XOR A," ++ (show n)
    XORHL -> "XOR A,(HL)"
    CP r -> "CP A," ++ (showRegister r)
    CPN n -> "CP A," ++ (showWord8 n)
    CPHL -> "CP A,(HL)"
    INC r -> "INC" &+ (showRegister r)
    INC2 rp -> "INC" &+ (showRegisterPair rp)
    INCHL -> "INC (HL)"
    DEC r -> "DEC" &+ (showRegister r)
    DEC2 rp -> "DEC" &+ (showRegisterPair rp)
    DECHL -> "DEC (HL)"
    ADD2HL rp -> "ADD HL," ++ (showRegisterPair rp)
    ADD2SP o -> "ADD SP," ++ (showOffset o)
    SWAP r -> "SWAP" &+ (showRegister r)
    SWAPHL -> "SWAP (HL)"
    DAA -> "DAA"
    CPL -> "CPL"
    CCF -> "CCF"
    SCF -> "SCF"
    NOP -> "NOP"
    HALT -> "HALT"
    STOP -> "STOP"
    DI -> "DI"
    EI -> "EI"
    RLCA -> "RLCA"
    RLA -> "RLA"
    RRCA -> "RRCA"
    RRA -> "RRA"
    RLC r -> "RLC" &+ (showRegister r)
    RLCHL -> "RLC (HL)"
    RL r -> "RL" &+ (showRegister r)
    RLHL -> "RL (HL)"
    RRC r -> "RRC" &+ (showRegister r)
    RRCHL -> "RRC (HL)"
    RR r -> "RR" &+ (showRegister r)
    RRHL -> "RR (HL)"
    SLA r -> "SLA" &+ (showRegister r)
    SLAHL -> "SLA (HL)"
    SRA r -> "SRA" &+ (showRegister r)
    SRAHL -> "SRA (HL)"
    SRL r -> "SRL" &+ (showRegister r)
    SRLHL -> "SRL (HL)"
    BIT bi r -> "BIT" &+ (showBitIndex bi) ++ "," ++ (showRegister r)
    BITHL bi -> "BIT" &+ (showBitIndex bi) ++ ",(HL)"
    SET bi r -> "SET" &+ (showBitIndex bi) ++ "," ++ (showRegister r)
    SETHL bi -> "SET" &+ (showBitIndex bi) ++ ",(HL)"
    RES bi r -> "RES" &+ (showBitIndex bi) ++ "," ++ (showRegister r)
    RESHL bi -> "RES" &+ (showBitIndex bi) ++ ",(HL)"
    JP c a -> case c of
                Nothing -> "JP" &+ (showAddress a)
                Just cc -> "JP" &+ (showFlagCondition cc) ++ "," ++ (showAddress a)
    JPHL -> "JP (HL)"
    JR c o -> case c of
                Nothing -> "JR" &+ (showOffset o)
                Just cc -> "JR" &+ (showFlagCondition cc) ++ "," ++ (showOffset o)
    CALL c a -> case c of
                  Nothing -> "CALL" &+ (showAddress a)
                  Just cc -> "CALL" &+ (showFlagCondition cc) ++ "," ++ (showAddress a)
    RST ra -> case ra of
                RST_00H -> "RST $00"
                RST_08H -> "RST $08"
                RST_10H -> "RST $10"
                RST_18H -> "RST $18"
                RST_20H -> "RST $20"
                RST_28H -> "RST $28"
                RST_30H -> "RST $30"
                RST_38H -> "RST $38"
    RET c -> case c of
               Nothing -> "RET"
               Just cc -> "RET" &+ showFlagCondition cc
    RETI -> "RETI"

    where
      x &+ y = x ++ " " ++ y
      showFlagCondition :: FlagCondition -> String
      showFlagCondition cc = case cc of
        FlagC -> "C"
        FlagNC -> "NC"
        FlagNZ -> "NZ"
        FlagZ -> "Z"
      showWord8 :: Word8 -> String
      showWord8 = showHex
      showAddress :: Word16 -> String
      showAddress a = showHex a
      showOffset :: Int8 -> String
      showOffset = show
      showBitIndex :: BitIndex -> String
      showBitIndex = show . intFromBitIndex
      showRegister :: Register -> String
      showRegister r = case r of
        A -> "A"
        B -> "B"
        C -> "C"
        D -> "D"
        E -> "E"
        H -> "H"
        L -> "L"
      showRegisterPair :: RegisterPair -> String
      showRegisterPair rp = case rp of
        BC -> "BC"
        DE -> "DE"
        HL -> "HL"
        SP -> "SP"
      showStackRegister :: StackRegister -> String
      showStackRegister sr = case sr of
        StackRegAF -> "AF"
        StackRegBC -> "BC"
        StackRegDE -> "DE"
        StackRegHL -> "HL"
        
intFromBitIndex :: forall t. (Num t) => BitIndex -> t
intFromBitIndex bi = case bi of
  Bit0 -> 0
  Bit1 -> 1
  Bit2 -> 2
  Bit3 -> 3
  Bit4 -> 4
  Bit5 -> 5
  Bit6 -> 6
  Bit7 -> 7

nFromRestartAddress :: forall t. (Num t) => RestartAddress -> t
nFromRestartAddress ra = case ra of
  RST_00H -> 0x0000
  RST_08H -> 0x0008
  RST_10H -> 0x0010
  RST_18H -> 0x0018
  RST_20H -> 0x0020
  RST_28H -> 0x0028
  RST_30H -> 0x0030
  RST_38H -> 0x0038

machineCodeToInstruction o a = let (i, _, _) = mcti o a in i

opcodeCycleCount :: Opcode -> CycleCount
opcodeCycleCount o = let (_, c, _) = mcti o (0,0) in c


mcti :: Opcode -> (Word8, Word16) -> (Instruction, CycleCount, ArgumentCount)
mcti 0x00 _      = (NOP,                                4,  0)
mcti 0x01 (_,nn) = (LD2 BC nn,                          12, 2)
mcti 0x02 _      = (LDPR True,                          8,  0)
mcti 0x03 _      = (INC2 BC,                            8,  0)
mcti 0x04 _      = (INC B,                              4,  0)
mcti 0x05 _      = (DEC B,                              4,  0)
mcti 0x06 (n,_)  = (LDRN B n,                           8,  1)
mcti 0x07 _      = (RLCA,                               4,  0)
mcti 0x08 (_,nn) = (LDP2 nn,                            20, 2)
mcti 0x09 _      = (ADD2HL BC,                          8,  0)
mcti 0x0A _      = (LDAP True,                          8,  0)
mcti 0x0B _      = (DEC2 BC,                            8,  0)
mcti 0x0C _      = (INC C,                              4,  0)
mcti 0x0D _      = (DEC C,                              4,  0)
mcti 0x0E (n,_)  = (LDRN C n,                           8,  1)
mcti 0x0F _      = (RRCA,                               4,  0)

mcti 0x10 _      = (STOP, {- TODO verify dummy arg=1 -} 4,  1)
mcti 0x11 (_,nn) = (LD2 DE nn,                          12, 2)
mcti 0x12 _      = (LDPR False,                         8,  0)
mcti 0x13 _      = (INC2 DE,                            8,  0)
mcti 0x14 _      = (INC D,                              4,  0)
mcti 0x15 _      = (DEC D,                              4,  0)
mcti 0x16 (n,_)  = (LDRN D n,                           8,  1)
mcti 0x17 _      = (RLA,                                4,  0)
mcti 0x18 (n,_)  = (JR Nothing (int8FromWord8 n),       8,  1)
mcti 0x19 _      = (ADD2HL DE,                          8,  0)
mcti 0x1A _      = (LDAP False,                         8,  0)
mcti 0x1B _      = (DEC2 DE,                            8,  0)
mcti 0x1C _      = (INC E,                              4,  0)
mcti 0x1D _      = (DEC E,                              4,  0)
mcti 0x1E (n,_)  = (LDRN E n,                           8,  1)
mcti 0x1F _      = (RRA,                                4,  0)

mcti 0x20 (n,_)  = (JR (Just FlagNZ) (int8FromWord8 n), 8,  1)
mcti 0x21 (_,nn) = (LD2 HL nn,                          12, 2)
mcti 0x22 _      = (LDI True,                           8,  0)
mcti 0x23 _      = (INC2 HL,                            8,  0)
mcti 0x24 _      = (INC H,                              4,  0)
mcti 0x25 _      = (DEC H,                              4,  0)
mcti 0x26 (n,_)  = (LDRN H n,                           8,  1)
mcti 0x27 _      = (DAA,                                4,  0)
mcti 0x28 (n,_)  = (JR (Just FlagZ) (int8FromWord8 n),  8,  1)
mcti 0x29 _      = (ADD2HL HL,                          8,  0)
mcti 0x2A _      = (LDI False,                          8,  0)
mcti 0x2B _      = (DEC2 HL,                            8,  0)
mcti 0x2C _      = (INC L,                              4,  0)
mcti 0x2D _      = (DEC L,                              4,  0)
mcti 0x2E (n,_)  = (LDRN L n,                           8,  1)
mcti 0x2F _      = (CPL,                                4,  0)

mcti 0x30 (n,_)  = (JR (Just FlagNC) (int8FromWord8 n), 8,  1)
mcti 0x31 (_,nn) = (LD2 SP nn,                          12, 2)
mcti 0x32 _      = (LDD True,                           8,  0)
mcti 0x33 _      = (INC2 SP,                            8,  0)
mcti 0x34 _      = (INCHL,                              12, 0)
mcti 0x35 _      = (DECHL,                              12, 0)
mcti 0x36 (n,_)  = (LDHLN n,                            12, 1)
mcti 0x37 _      = (SCF,                                4,  0)
mcti 0x38 (n,_)  = (JR (Just FlagC) (int8FromWord8 n),  8,  1)
mcti 0x39 _      = (ADD2HL SP,                          8,  0)
mcti 0x3A _      = (LDD False,                          8,  0)
mcti 0x3B _      = (DEC2 SP,                            8,  0)
mcti 0x3C _      = (INC A,                              4,  0)
mcti 0x3D _      = (DEC A,                              4,  0)
mcti 0x3E (n,_)  = (LDRN A n,                           8,  1)
mcti 0x3F _      = (CCF,                                4,  0)

mcti 0x40 _      = (LDR B B,                            4,  0)
mcti 0x41 _      = (LDR B C,                            4,  0)
mcti 0x42 _      = (LDR B D,                            4,  0)
mcti 0x43 _      = (LDR B E,                            4,  0)
mcti 0x44 _      = (LDR B H,                            4,  0)
mcti 0x45 _      = (LDR B L,                            4,  0)
mcti 0x46 _      = (LDRHL B,                            8,  0)
mcti 0x47 _      = (LDR B A,                            4,  0)
mcti 0x48 _      = (LDR C B,                            4,  0)
mcti 0x49 _      = (LDR C C,                            4,  0)
mcti 0x4A _      = (LDR C D,                            4,  0)
mcti 0x4B _      = (LDR C E,                            4,  0)
mcti 0x4C _      = (LDR C H,                            4,  0)
mcti 0x4D _      = (LDR C L,                            4,  0)
mcti 0x4E _      = (LDRHL C,                            8,  0)
mcti 0x4F _      = (LDR C A,                            4,  0)

mcti 0x50 _      = (LDR D B,                            4,  0)
mcti 0x51 _      = (LDR D C,                            4,  0)
mcti 0x52 _      = (LDR D D,                            4,  0)
mcti 0x53 _      = (LDR D E,                            4,  0)
mcti 0x54 _      = (LDR D H,                            4,  0)
mcti 0x55 _      = (LDR D L,                            4,  0)
mcti 0x56 _      = (LDRHL D,                            8,  0)
mcti 0x57 _      = (LDR D A,                            4,  0)
mcti 0x58 _      = (LDR E B,                            4,  0)
mcti 0x59 _      = (LDR E C,                            4,  0)
mcti 0x5A _      = (LDR E D,                            4,  0)
mcti 0x5B _      = (LDR E E,                            4,  0)
mcti 0x5C _      = (LDR E H,                            4,  0)
mcti 0x5D _      = (LDR E L,                            4,  0)
mcti 0x5E _      = (LDRHL E,                            8,  0)
mcti 0x5F _      = (LDR E A,                            4,  0)
                           
mcti 0x60 _      = (LDR H B,                            4,  0)
mcti 0x61 _      = (LDR H C,                            4,  0)
mcti 0x62 _      = (LDR H D,                            4,  0)
mcti 0x63 _      = (LDR H E,                            4,  0)
mcti 0x64 _      = (LDR H H,                            4,  0)
mcti 0x65 _      = (LDR H L,                            4,  0)
mcti 0x66 _      = (LDRHL H,                            8,  0)
mcti 0x67 _      = (LDR H A,                            4,  0)
mcti 0x68 _      = (LDR L B,                            4,  0)
mcti 0x69 _      = (LDR L C,                            4,  0)
mcti 0x6A _      = (LDR L D,                            4,  0)
mcti 0x6B _      = (LDR L E,                            4,  0)
mcti 0x6C _      = (LDR L H,                            4,  0)
mcti 0x6D _      = (LDR L L,                            4,  0)
mcti 0x6E _      = (LDRHL L,                            8,  0)
mcti 0x6F _      = (LDR L A,                            4,  0)
                           
mcti 0x70 _      = (LDHL B,                             8,  0)
mcti 0x71 _      = (LDHL C,                             8,  0)
mcti 0x72 _      = (LDHL D,                             8,  0)
mcti 0x73 _      = (LDHL E,                             8,  0)
mcti 0x74 _      = (LDHL H,                             8,  0)
mcti 0x75 _      = (LDHL L,                             8,  0)
mcti 0x76 _      = (HALT,                               4,  0)
mcti 0x77 _      = (LDHL A,                             8,  0)
mcti 0x78 _      = (LDR A B,                            4,  0)
mcti 0x79 _      = (LDR A C,                            4,  0)
mcti 0x7A _      = (LDR A D,                            4,  0)
mcti 0x7B _      = (LDR A E,                            4,  0)
mcti 0x7C _      = (LDR A H,                            4,  0)
mcti 0x7D _      = (LDR A L,                            4,  0)
mcti 0x7E _      = (LDRHL A,                            8,  0)
mcti 0x7F _      = (LDR A A,                            4,  0)

mcti 0x80 _      = (ADD B,                              4,  0)
mcti 0x81 _      = (ADD C,                              4,  0)
mcti 0x82 _      = (ADD D,                              4,  0)
mcti 0x83 _      = (ADD E,                              4,  0)
mcti 0x84 _      = (ADD H,                              4,  0)
mcti 0x85 _      = (ADD L,                              4,  0)
mcti 0x86 _      = (ADDHL,                              8,  0)
mcti 0x87 _      = (ADD A,                              4,  0)
mcti 0x88 _      = (ADC B,                              4,  0)
mcti 0x89 _      = (ADC C,                              4,  0)
mcti 0x8A _      = (ADC D,                              4,  0)
mcti 0x8B _      = (ADC E,                              4,  0)
mcti 0x8C _      = (ADC H,                              4,  0)
mcti 0x8D _      = (ADC L,                              4,  0)
mcti 0x8E _      = (ADCHL,                              8,  0)
mcti 0x8F _      = (ADC A,                              4,  0)

mcti 0x90 _      = (SUB B,                              4,  0)
mcti 0x91 _      = (SUB C,                              4,  0)
mcti 0x92 _      = (SUB D,                              4,  0)
mcti 0x93 _      = (SUB E,                              4,  0)
mcti 0x94 _      = (SUB H,                              4,  0)
mcti 0x95 _      = (SUB L,                              4,  0)
mcti 0x96 _      = (SUBHL,                              8,  0)
mcti 0x97 _      = (SUB A,                              4,  0)
mcti 0x98 _      = (SBC B,                              4,  0)
mcti 0x99 _      = (SBC C,                              4,  0)
mcti 0x9A _      = (SBC D,                              4,  0)
mcti 0x9B _      = (SBC E,                              4,  0)
mcti 0x9C _      = (SBC H,                              4,  0)
mcti 0x9D _      = (SBC L,                              4,  0)
mcti 0x9E _      = (SBCHL,                              8,  0)
mcti 0x9F _      = (SBC A,                              4,  0)

mcti 0xA0 _      = (AND B,                              4,  0)
mcti 0xA1 _      = (AND C,                              4,  0)
mcti 0xA2 _      = (AND D,                              4,  0)
mcti 0xA3 _      = (AND E,                              4,  0)
mcti 0xA4 _      = (AND H,                              4,  0)
mcti 0xA5 _      = (AND L,                              4,  0)
mcti 0xA6 _      = (ANDHL,                              8,  0)
mcti 0xA7 _      = (AND A,                              4,  0)
mcti 0xA8 _      = (XOR B,                              4,  0)
mcti 0xA9 _      = (XOR C,                              4,  0)
mcti 0xAA _      = (XOR D,                              4,  0)
mcti 0xAB _      = (XOR E,                              4,  0)
mcti 0xAC _      = (XOR H,                              4,  0)
mcti 0xAD _      = (XOR L,                              4,  0)
mcti 0xAE _      = (XORHL,                              8,  0)
mcti 0xAF _      = (XOR A,                              4,  0)

mcti 0xB0 _      = (OR B,                               4,  0)
mcti 0xB1 _      = (OR C,                               4,  0)
mcti 0xB2 _      = (OR D,                               4,  0)
mcti 0xB3 _      = (OR E,                               4,  0)
mcti 0xB4 _      = (OR H,                               4,  0)
mcti 0xB5 _      = (OR L,                               4,  0)
mcti 0xB6 _      = (ORHL,                               8,  0)
mcti 0xB7 _      = (OR A,                               4,  0)
mcti 0xB8 _      = (CP B,                               4,  0)
mcti 0xB9 _      = (CP C,                               4,  0)
mcti 0xBA _      = (CP D,                               4,  0)
mcti 0xBB _      = (CP E,                               4,  0)
mcti 0xBC _      = (CP H,                               4,  0)
mcti 0xBD _      = (CP L,                               4,  0)
mcti 0xBE _      = (CPHL,                               8,  0)
mcti 0xBF _      = (CP A,                               4,  0)

mcti 0xC0 _      = (RET (Just FlagNZ),                  8,  0)
mcti 0xC1 _      = (POP StackRegBC,                     12, 0)
mcti 0xC2 (_,nn) = (JP (Just FlagNZ) nn,                12, 2)
mcti 0xC3 (_,nn) = (JP Nothing nn,                      12, 2)
mcti 0xC4 (_,nn) = (CALL (Just FlagNZ) nn,              12, 2)
mcti 0xC5 _      = (PUSH StackRegBC,                    16, 0)
mcti 0xC6 (n,_)  = (ADDN n,                             8,  1)
mcti 0xC7 _      = (RST RST_00H,                        32, 0)
mcti 0xC8 _      = (RET (Just FlagZ),                   8,  0)
mcti 0xC9 _      = (RET Nothing,                        8,  0)
mcti 0xCA (_,nn) = (JP (Just FlagZ) nn,                 12,  2)
mcti 0xCB (n,_)  = let (i, c) = mctiCB n in (i, c, 1)
mcti 0xCC (_,nn) = (CALL (Just FlagZ) nn,               12, 2)
mcti 0xCD (_,nn) = (CALL Nothing nn,                    12, 2)
mcti 0xCE (n,_)  = (ADCN n,                             8,  1)
mcti 0xCF _      = (RST RST_08H,                        32, 0)

mcti 0xD0 _      = (RET (Just FlagNC),                  8,  0)
mcti 0xD1 _      = (POP StackRegDE,                     12, 0)
mcti 0xD2 (_,nn) = (JP (Just FlagNC) nn,                12, 2)
mcti 0xD3 _      = error "$D3 Invalid Opcode"
mcti 0xD4 (_,nn) = (CALL (Just FlagNC) nn,              12, 2)
mcti 0xD5 _      = (PUSH StackRegDE,                    16, 0)
mcti 0xD6 (n,_)  = (SUBN n,                             8,  1)
mcti 0xD7 _      = (RST RST_10H,                        32, 0)
mcti 0xD8 _      = (RET (Just FlagC),                   8,  0)
mcti 0xD9 _      = (RETI,                               8,  0)
mcti 0xDA (_,nn) = (JP (Just FlagC) nn,                 12, 2)
mcti 0xDB _      = error "$DB Invalid Opcode"
mcti 0xDC (_,nn) = (CALL (Just FlagC) nn,               12, 2)
mcti 0xDD _      = error "$DD Invalid Opcode"
mcti 0xDE (n,_)  = (SBCN n,                             8,  1)
mcti 0xDF _      = (RST RST_18H,                        32, 0)

mcti 0xE0 (n,_)  = (LDH True n,                         12, 1)
mcti 0xE1 _      = (POP StackRegHL,                     12, 0)
mcti 0xE2 _      = (LDPC,                               8,  0)
mcti 0xE3 _      = error "$E3 Invalid Opcode"
mcti 0xE4 _      = error "$E4 Invalid Opcode"
mcti 0xE5 _      = (PUSH StackRegHL,                    16, 0)
mcti 0xE6 (n,_)  = (ANDN n,                             8,  1)
mcti 0xE7 _      = (RST RST_20H,                        32, 0)
mcti 0xE8 (n,_)  = (ADD2SP (int8FromWord8 n),           16, 1)
mcti 0xE9 _      = (JPHL,                               4,  0)
mcti 0xEA (_,nn) = (LDPN nn,                            16, 2)
mcti 0xEB _      = error "$EB Invalid Opcode"
mcti 0xEC _      = error "$EC Invalid Opcode"
mcti 0xED _      = error "$ED Invalid Opcode"
mcti 0xEE (n,_)  = (XORN n,                             8,  1)
mcti 0xEF _      = (RST RST_28H,                        32, 0)

mcti 0xF0 (n,_)  = (LDH False n,                        12, 1)
mcti 0xF1 _      = (POP StackRegAF,                     12, 0)
mcti 0xF2 _      = (LDAC,                               8,  0)
mcti 0xF3 _      = (DI,                                 4,  0)
mcti 0xF4 _      = error "$F4 Invalid Opcode"
mcti 0xF5 _      = (PUSH StackRegAF,                    16, 0)
mcti 0xF6 (n,_)  = (ORN n,                              8,  1)
mcti 0xF7 _      = (RST RST_30H,                        32, 0)
mcti 0xF8 (n,_)  = (LDHL2 (int8FromWord8 n),            12, 1)
mcti 0xF9 _      = (LDSP2,                              8,  0)
mcti 0xFA (_,nn) = (LDAPN nn,                           16, 2)
mcti 0xFB _      = (EI,                                 4,  0)
mcti 0xFC _      = error "$FC Invalid Opcode"
mcti 0xFD _      = error "$FD Invalid Opcode"
mcti 0xFE (n,_)  = (CPN n,                              8,  1)
mcti 0xFF _      = (RST RST_38H,                        32, 0)

mctiCB :: Word8 -> (Instruction, CycleCount)
mctiCB 0x00 = (RLC B, 8)
mctiCB 0x01 = (RLC C, 8)
mctiCB 0x02 = (RLC D, 8)
mctiCB 0x03 = (RLC E, 8)
mctiCB 0x04 = (RLC H, 8)
mctiCB 0x05 = (RLC L, 8)
mctiCB 0x06 = (RLCHL, 16)
mctiCB 0x07 = (RLC A, 8)
mctiCB 0x08 = (RRC B, 8)
mctiCB 0x09 = (RRC C, 8)
mctiCB 0x0A = (RRC D, 8)
mctiCB 0x0B = (RRC E, 8)
mctiCB 0x0C = (RRC H, 8)
mctiCB 0x0D = (RRC L, 8)
mctiCB 0x0E = (RRCHL, 16)
mctiCB 0x0F = (RRC A, 8)

mctiCB 0x10 = (RL B, 8)
mctiCB 0x11 = (RL C, 8)
mctiCB 0x12 = (RL D, 8)
mctiCB 0x13 = (RL E, 8)
mctiCB 0x14 = (RL H, 8)
mctiCB 0x15 = (RL L, 8)
mctiCB 0x16 = (RLHL, 16)
mctiCB 0x17 = (RL A, 8)
mctiCB 0x18 = (RR B, 8)
mctiCB 0x19 = (RR C, 8)
mctiCB 0x1A = (RR D, 8)
mctiCB 0x1B = (RR E, 8)
mctiCB 0x1C = (RR H, 8)
mctiCB 0x1D = (RR L, 8)
mctiCB 0x1E = (RRHL, 16)
mctiCB 0x1F = (RR A, 8)

mctiCB 0x20 = (SLA B, 8)
mctiCB 0x21 = (SLA C, 8)
mctiCB 0x22 = (SLA D, 8)
mctiCB 0x23 = (SLA E, 8)
mctiCB 0x24 = (SLA H, 8)
mctiCB 0x25 = (SLA L, 8)
mctiCB 0x26 = (SLAHL, 16)
mctiCB 0x27 = (SLA A, 8)
mctiCB 0x28 = (SRA B, 8)
mctiCB 0x29 = (SRA C, 8)
mctiCB 0x2A = (SRA D, 8)
mctiCB 0x2B = (SRA E, 8)
mctiCB 0x2C = (SRA H, 8)
mctiCB 0x2D = (SRA L, 8)
mctiCB 0x2E = (SRAHL, 16)
mctiCB 0x2F = (SRA A, 8)

mctiCB 0x30 = (SWAP B, 8)
mctiCB 0x31 = (SWAP C, 8)
mctiCB 0x32 = (SWAP D, 8)
mctiCB 0x33 = (SWAP E, 8)
mctiCB 0x34 = (SWAP H, 8)
mctiCB 0x35 = (SWAP L, 8)
mctiCB 0x36 = (SWAPHL, 16)
mctiCB 0x37 = (SWAP A, 8)
mctiCB 0x38 = (SRL B, 8)
mctiCB 0x39 = (SRL C, 8)
mctiCB 0x3A = (SRL D, 8)
mctiCB 0x3B = (SRL E, 8)
mctiCB 0x3C = (SRL H, 8)
mctiCB 0x3D = (SRL L, 8)
mctiCB 0x3E = (SRLHL, 16)
mctiCB 0x3F = (SRL A, 8)

mctiCB 0x40 = (BIT Bit0 B, 8)
mctiCB 0x41 = (BIT Bit0 C, 8)
mctiCB 0x42 = (BIT Bit0 D, 8)
mctiCB 0x43 = (BIT Bit0 E, 8)
mctiCB 0x44 = (BIT Bit0 H, 8)
mctiCB 0x45 = (BIT Bit0 L, 8)
mctiCB 0x46 = (BITHL Bit0, 16)
mctiCB 0x47 = (BIT Bit0 A, 8)
mctiCB 0x48 = (BIT Bit1 B, 8)
mctiCB 0x49 = (BIT Bit1 C, 8)
mctiCB 0x4A = (BIT Bit1 D, 8)
mctiCB 0x4B = (BIT Bit1 E, 8)
mctiCB 0x4C = (BIT Bit1 H, 8)
mctiCB 0x4D = (BIT Bit1 L, 8)
mctiCB 0x4E = (BITHL Bit1, 16)
mctiCB 0x4F = (BIT Bit1 A, 8)

mctiCB 0x50 = (BIT Bit2 B, 8)
mctiCB 0x51 = (BIT Bit2 C, 8)
mctiCB 0x52 = (BIT Bit2 D, 8)
mctiCB 0x53 = (BIT Bit2 E, 8)
mctiCB 0x54 = (BIT Bit2 H, 8)
mctiCB 0x55 = (BIT Bit2 L, 8)
mctiCB 0x56 = (BITHL Bit2, 16)
mctiCB 0x57 = (BIT Bit2 A, 8)
mctiCB 0x58 = (BIT Bit3 B, 8)
mctiCB 0x59 = (BIT Bit3 C, 8)
mctiCB 0x5A = (BIT Bit3 D, 8)
mctiCB 0x5B = (BIT Bit3 E, 8)
mctiCB 0x5C = (BIT Bit3 H, 8)
mctiCB 0x5D = (BIT Bit3 L, 8)
mctiCB 0x5E = (BITHL Bit3, 16)
mctiCB 0x5F = (BIT Bit3 A, 8)

mctiCB 0x60 = (BIT Bit4 B, 8)
mctiCB 0x61 = (BIT Bit4 C, 8)
mctiCB 0x62 = (BIT Bit4 D, 8)
mctiCB 0x63 = (BIT Bit4 E, 8)
mctiCB 0x64 = (BIT Bit4 H, 8)
mctiCB 0x65 = (BIT Bit4 L, 8)
mctiCB 0x66 = (BITHL Bit4, 16)
mctiCB 0x67 = (BIT Bit4 A, 8)
mctiCB 0x68 = (BIT Bit5 B, 8)
mctiCB 0x69 = (BIT Bit5 C, 8)
mctiCB 0x6A = (BIT Bit5 D, 8)
mctiCB 0x6B = (BIT Bit5 E, 8)
mctiCB 0x6C = (BIT Bit5 H, 8)
mctiCB 0x6D = (BIT Bit5 L, 8)
mctiCB 0x6E = (BITHL Bit5, 16)
mctiCB 0x6F = (BIT Bit5 A, 8)

mctiCB 0x70 = (BIT Bit6 B, 8)
mctiCB 0x71 = (BIT Bit6 C, 8)
mctiCB 0x72 = (BIT Bit6 D, 8)
mctiCB 0x73 = (BIT Bit6 E, 8)
mctiCB 0x74 = (BIT Bit6 H, 8)
mctiCB 0x75 = (BIT Bit6 L, 8)
mctiCB 0x76 = (BITHL Bit6, 16)
mctiCB 0x77 = (BIT Bit6 A, 8)
mctiCB 0x78 = (BIT Bit7 B, 8)
mctiCB 0x79 = (BIT Bit7 C, 8)
mctiCB 0x7A = (BIT Bit7 D, 8)
mctiCB 0x7B = (BIT Bit7 E, 8)
mctiCB 0x7C = (BIT Bit7 H, 8)
mctiCB 0x7D = (BIT Bit7 L, 8)
mctiCB 0x7E = (BITHL Bit7, 16)
mctiCB 0x7F = (BIT Bit7 A, 8)

mctiCB 0x80 = (RES Bit0 B, 8)
mctiCB 0x81 = (RES Bit0 C, 8)
mctiCB 0x82 = (RES Bit0 D, 8)
mctiCB 0x83 = (RES Bit0 E, 8)
mctiCB 0x84 = (RES Bit0 H, 8)
mctiCB 0x85 = (RES Bit0 L, 8)
mctiCB 0x86 = (RESHL Bit0, 16)
mctiCB 0x87 = (RES Bit0 A, 8)
mctiCB 0x88 = (RES Bit1 B, 8)
mctiCB 0x89 = (RES Bit1 C, 8)
mctiCB 0x8A = (RES Bit1 D, 8)
mctiCB 0x8B = (RES Bit1 E, 8)
mctiCB 0x8C = (RES Bit1 H, 8)
mctiCB 0x8D = (RES Bit1 L, 8)
mctiCB 0x8E = (RESHL Bit1, 16)
mctiCB 0x8F = (RES Bit1 A, 8)

mctiCB 0x90 = (RES Bit2 B, 8)
mctiCB 0x91 = (RES Bit2 C, 8)
mctiCB 0x92 = (RES Bit2 D, 8)
mctiCB 0x93 = (RES Bit2 E, 8)
mctiCB 0x94 = (RES Bit2 H, 8)
mctiCB 0x95 = (RES Bit2 L, 8)
mctiCB 0x96 = (RESHL Bit2, 16)
mctiCB 0x97 = (RES Bit2 A, 8)
mctiCB 0x98 = (RES Bit3 B, 8)
mctiCB 0x99 = (RES Bit3 C, 8)
mctiCB 0x9A = (RES Bit3 D, 8)
mctiCB 0x9B = (RES Bit3 E, 8)
mctiCB 0x9C = (RES Bit3 H, 8)
mctiCB 0x9D = (RES Bit3 L, 8)
mctiCB 0x9E = (RESHL Bit3, 16)
mctiCB 0x9F = (RES Bit3 A, 8)

mctiCB 0xA0 = (RES Bit4 B, 8)
mctiCB 0xA1 = (RES Bit4 C, 8)
mctiCB 0xA2 = (RES Bit4 D, 8)
mctiCB 0xA3 = (RES Bit4 E, 8)
mctiCB 0xA4 = (RES Bit4 H, 8)
mctiCB 0xA5 = (RES Bit4 L, 8)
mctiCB 0xA6 = (RESHL Bit4, 16)
mctiCB 0xA7 = (RES Bit4 A, 8)
mctiCB 0xA8 = (RES Bit5 B, 8)
mctiCB 0xA9 = (RES Bit5 C, 8)
mctiCB 0xAA = (RES Bit5 D, 8)
mctiCB 0xAB = (RES Bit5 E, 8)
mctiCB 0xAC = (RES Bit5 H, 8)
mctiCB 0xAD = (RES Bit5 L, 8)
mctiCB 0xAE = (RESHL Bit5, 16)
mctiCB 0xAF = (RES Bit5 A, 8)

mctiCB 0xB0 = (RES Bit6 B, 8)
mctiCB 0xB1 = (RES Bit6 C, 8)
mctiCB 0xB2 = (RES Bit6 D, 8)
mctiCB 0xB3 = (RES Bit6 E, 8)
mctiCB 0xB4 = (RES Bit6 H, 8)
mctiCB 0xB5 = (RES Bit6 L, 8)
mctiCB 0xB6 = (RESHL Bit6, 16)
mctiCB 0xB7 = (RES Bit6 A, 8)
mctiCB 0xB8 = (RES Bit7 B, 8)
mctiCB 0xB9 = (RES Bit7 C, 8)
mctiCB 0xBA = (RES Bit7 D, 8)
mctiCB 0xBB = (RES Bit7 E, 8)
mctiCB 0xBC = (RES Bit7 H, 8)
mctiCB 0xBD = (RES Bit7 L, 8)
mctiCB 0xBE = (RESHL Bit7, 16)
mctiCB 0xBF = (RES Bit7 A, 8)

mctiCB 0xC0 = (SET Bit0 B, 8)
mctiCB 0xC1 = (SET Bit0 C, 8)
mctiCB 0xC2 = (SET Bit0 D, 8)
mctiCB 0xC3 = (SET Bit0 E, 8)
mctiCB 0xC4 = (SET Bit0 H, 8)
mctiCB 0xC5 = (SET Bit0 L, 8)
mctiCB 0xC6 = (SETHL Bit0, 16)
mctiCB 0xC7 = (SET Bit0 A, 8)
mctiCB 0xC8 = (SET Bit1 B, 8)
mctiCB 0xC9 = (SET Bit1 C, 8)
mctiCB 0xCA = (SET Bit1 D, 8)
mctiCB 0xCB = (SET Bit1 E, 8)
mctiCB 0xCC = (SET Bit1 H, 8)
mctiCB 0xCD = (SET Bit1 L, 8)
mctiCB 0xCE = (SETHL Bit1, 16)
mctiCB 0xCF = (SET Bit1 A, 8)

mctiCB 0xD0 = (SET Bit2 B, 8)
mctiCB 0xD1 = (SET Bit2 C, 8)
mctiCB 0xD2 = (SET Bit2 D, 8)
mctiCB 0xD3 = (SET Bit2 E, 8)
mctiCB 0xD4 = (SET Bit2 H, 8)
mctiCB 0xD5 = (SET Bit2 L, 8)
mctiCB 0xD6 = (SETHL Bit2, 16)
mctiCB 0xD7 = (SET Bit2 A, 8)
mctiCB 0xD8 = (SET Bit3 B, 8)
mctiCB 0xD9 = (SET Bit3 C, 8)
mctiCB 0xDA = (SET Bit3 D, 8)
mctiCB 0xDB = (SET Bit3 E, 8)
mctiCB 0xDC = (SET Bit3 H, 8)
mctiCB 0xDD = (SET Bit3 L, 8)
mctiCB 0xDE = (SETHL Bit3, 16)
mctiCB 0xDF = (SET Bit3 A, 8)

mctiCB 0xE0 = (SET Bit4 B, 8)
mctiCB 0xE1 = (SET Bit4 C, 8)
mctiCB 0xE2 = (SET Bit4 D, 8)
mctiCB 0xE3 = (SET Bit4 E, 8)
mctiCB 0xE4 = (SET Bit4 H, 8)
mctiCB 0xE5 = (SET Bit4 L, 8)
mctiCB 0xE6 = (SETHL Bit4, 16)
mctiCB 0xE7 = (SET Bit4 A, 8)
mctiCB 0xE8 = (SET Bit5 B, 8)
mctiCB 0xE9 = (SET Bit5 C, 8)
mctiCB 0xEA = (SET Bit5 D, 8)
mctiCB 0xEB = (SET Bit5 E, 8)
mctiCB 0xEC = (SET Bit5 H, 8)
mctiCB 0xED = (SET Bit5 L, 8)
mctiCB 0xEE = (SETHL Bit5, 16)
mctiCB 0xEF = (SET Bit5 A, 8)

mctiCB 0xF0 = (SET Bit6 B, 8)
mctiCB 0xF1 = (SET Bit6 C, 8)
mctiCB 0xF2 = (SET Bit6 D, 8)
mctiCB 0xF3 = (SET Bit6 E, 8)
mctiCB 0xF4 = (SET Bit6 H, 8)
mctiCB 0xF5 = (SET Bit6 L, 8)
mctiCB 0xF6 = (SETHL Bit6, 16)
mctiCB 0xF7 = (SET Bit6 A, 8)
mctiCB 0xF8 = (SET Bit7 B, 8)
mctiCB 0xF9 = (SET Bit7 C, 8)
mctiCB 0xFA = (SET Bit7 D, 8)
mctiCB 0xFB = (SET Bit7 E, 8)
mctiCB 0xFC = (SET Bit7 H, 8)
mctiCB 0xFD = (SET Bit7 L, 8)
mctiCB 0xFE = (SETHL Bit7, 16)
mctiCB 0xFF = (SET Bit7 A, 8)

