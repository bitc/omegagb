-- Copyright 2006 mutantlemon.com

module CpuIO where

import Data.Word
import Data.Int
import Data.Bits
import Data.IORef

import MachineStateIO
import WordUtil

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

machineUpdateInstructionIO :: MachineStateIO -> IO ()
machineUpdateInstructionIO s = do
  pc <- readIORef (msRegPC s)
  opcode <- readMemoryIO s pc
  n <- readMemoryIO s (pc + 1)
  n' <- readMemoryIO s (pc + 2)
  let nn = joinWord16 n' n

  case opcode of
    0x00 -> execNOP
    0x01 -> execLD2 BC nn
    0x02 -> execLDPR True
    0x03 -> execINC2 BC
    0x04 -> execINC B
    0x05 -> execDEC B
    0x06 -> execLDRN B n
    0x07 -> execRLCA
    0x08 -> execLDP2 nn
    0x09 -> execADD2HL BC
    0x0A -> execLDAP True
    0x0B -> execDEC2 BC
    0x0C -> execINC C
    0x0D -> execDEC C
    0x0E -> execLDRN C n
    0x0F -> execRRCA

    0x10 -> execSTOP
    0x11 -> execLD2 DE nn
    0x12 -> execLDPR False
    0x13 -> execINC2 DE
    0x14 -> execINC D
    0x15 -> execDEC D
    0x16 -> execLDRN D n
    0x17 -> execRLA
    0x18 -> execJR Nothing (fromIntegral n)
    0x19 -> execADD2HL DE
    0x1A -> execLDAP False
    0x1B -> execDEC2 DE
    0x1C -> execINC E
    0x1D -> execDEC E
    0x1E -> execLDRN E n
    0x1F -> execRRA

    0x20 -> execJR (Just FlagNZ) (fromIntegral n)
    0x21 -> execLD2 HL nn
    0x22 -> execLDI True
    0x23 -> execINC2 HL
    0x24 -> execINC H
    0x25 -> execDEC H
    0x26 -> execLDRN H n
    0x27 -> execDAA
    0x28 -> execJR (Just FlagZ) (fromIntegral n)
    0x29 -> execADD2HL HL
    0x2A -> execLDI False
    0x2B -> execDEC2 HL
    0x2C -> execINC L
    0x2D -> execDEC L
    0x2E -> execLDRN L n
    0x2F -> execCPL
    
    0x30 -> execJR (Just FlagNC) (fromIntegral n)
    0x31 -> execLD2 SP nn
    0x32 -> execLDD True
    0x33 -> execINC2 SP
    0x34 -> execINCHL
    0x35 -> execDECHL
    0x36 -> execLDHLN n
    0x37 -> execSCF
    0x38 -> execJR (Just FlagC) (fromIntegral n)
    0x39 -> execADD2HL SP
    0x3A -> execLDD False
    0x3B -> execDEC2 SP
    0x3C -> execINC A
    0x3D -> execDEC A
    0x3E -> execLDRN A n
    0x3F -> execCCF

    0x40 -> execLDR B B
    0x41 -> execLDR B C
    0x42 -> execLDR B D
    0x43 -> execLDR B E
    0x44 -> execLDR B H
    0x45 -> execLDR B L
    0x46 -> execLDRHL B
    0x47 -> execLDR B A
    0x48 -> execLDR C B
    0x49 -> execLDR C C
    0x4A -> execLDR C D
    0x4B -> execLDR C E
    0x4C -> execLDR C H
    0x4D -> execLDR C L
    0x4E -> execLDRHL C
    0x4F -> execLDR C A

    0x50 -> execLDR D B
    0x51 -> execLDR D C
    0x52 -> execLDR D D
    0x53 -> execLDR D E
    0x54 -> execLDR D H
    0x55 -> execLDR D L
    0x56 -> execLDRHL D
    0x57 -> execLDR D A
    0x58 -> execLDR E B
    0x59 -> execLDR E C
    0x5A -> execLDR E D
    0x5B -> execLDR E E
    0x5C -> execLDR E H
    0x5D -> execLDR E L
    0x5E -> execLDRHL E
    0x5F -> execLDR E A
                           
    0x60 -> execLDR H B
    0x61 -> execLDR H C
    0x62 -> execLDR H D
    0x63 -> execLDR H E
    0x64 -> execLDR H H
    0x65 -> execLDR H L
    0x66 -> execLDRHL H
    0x67 -> execLDR H A
    0x68 -> execLDR L B
    0x69 -> execLDR L C
    0x6A -> execLDR L D
    0x6B -> execLDR L E
    0x6C -> execLDR L H
    0x6D -> execLDR L L
    0x6E -> execLDRHL L
    0x6F -> execLDR L A
                           
    0x70 -> execLDHL B
    0x71 -> execLDHL C
    0x72 -> execLDHL D
    0x73 -> execLDHL E
    0x74 -> execLDHL H
    0x75 -> execLDHL L
    0x76 -> execHALT
    0x77 -> execLDHL A
    0x78 -> execLDR A B
    0x79 -> execLDR A C
    0x7A -> execLDR A D
    0x7B -> execLDR A E
    0x7C -> execLDR A H
    0x7D -> execLDR A L
    0x7E -> execLDRHL A
    0x7F -> execLDR A A

    0x80 -> execADD B
    0x81 -> execADD C
    0x82 -> execADD D
    0x83 -> execADD E
    0x84 -> execADD H
    0x85 -> execADD L
    0x86 -> execADDHL
    0x87 -> execADD A
    0x88 -> execADC B
    0x89 -> execADC C
    0x8A -> execADC D
    0x8B -> execADC E
    0x8C -> execADC H
    0x8D -> execADC L
    0x8E -> execADCHL
    0x8F -> execADC A

    0x90 -> execSUB B
    0x91 -> execSUB C
    0x92 -> execSUB D
    0x93 -> execSUB E
    0x94 -> execSUB H
    0x95 -> execSUB L
    0x96 -> execSUBHL
    0x97 -> execSUB A
    0x98 -> execSBC B
    0x99 -> execSBC C
    0x9A -> execSBC D
    0x9B -> execSBC E
    0x9C -> execSBC H
    0x9D -> execSBC L
    0x9E -> execSBCHL
    0x9F -> execSBC A

    0xA0 -> execAND B
    0xA1 -> execAND C
    0xA2 -> execAND D
    0xA3 -> execAND E
    0xA4 -> execAND H
    0xA5 -> execAND L
    0xA6 -> execANDHL
    0xA7 -> execAND A
    0xA8 -> execXOR B
    0xA9 -> execXOR C
    0xAA -> execXOR D
    0xAB -> execXOR E
    0xAC -> execXOR H
    0xAD -> execXOR L
    0xAE -> execXORHL
    0xAF -> execXOR A

    0xB0 -> execOR B
    0xB1 -> execOR C
    0xB2 -> execOR D
    0xB3 -> execOR E
    0xB4 -> execOR H
    0xB5 -> execOR L
    0xB6 -> execORHL
    0xB7 -> execOR A
    0xB8 -> execCP B
    0xB9 -> execCP C
    0xBA -> execCP D
    0xBB -> execCP E
    0xBC -> execCP H
    0xBD -> execCP L
    0xBE -> execCPHL
    0xBF -> execCP A

    0xC0 -> execRET (Just FlagNZ)
    0xC1 -> execPOP StackRegBC
    0xC2 -> execJP (Just FlagNZ) nn
    0xC3 -> execJP Nothing nn
    0xC4 -> execCALL (Just FlagNZ) nn
    0xC5 -> execPUSH StackRegBC
    0xC6 -> execADDN n
    0xC7 -> execRST RST_00H
    0xC8 -> execRET (Just FlagZ)
    0xC9 -> execRET Nothing
    0xCA -> execJP (Just FlagZ) nn
    0xCB -> case n of
              0x00 -> execRLC B
              0x01 -> execRLC C
              0x02 -> execRLC D
              0x03 -> execRLC E
              0x04 -> execRLC H
              0x05 -> execRLC L
              0x06 -> execRLCHL
              0x07 -> execRLC A
              0x08 -> execRRC B
              0x09 -> execRRC C
              0x0A -> execRRC D
              0x0B -> execRRC E
              0x0C -> execRRC H
              0x0D -> execRRC L
              0x0E -> execRRCHL
              0x0F -> execRRC A

              0x10 -> execRL B
              0x11 -> execRL C
              0x12 -> execRL D
              0x13 -> execRL E
              0x14 -> execRL H
              0x15 -> execRL L
              0x16 -> execRLHL
              0x17 -> execRL A
              0x18 -> execRR B
              0x19 -> execRR C
              0x1A -> execRR D
              0x1B -> execRR E
              0x1C -> execRR H
              0x1D -> execRR L
              0x1E -> execRRHL
              0x1F -> execRR A

              0x20 -> execSLA B
              0x21 -> execSLA C
              0x22 -> execSLA D
              0x23 -> execSLA E
              0x24 -> execSLA H
              0x25 -> execSLA L
              0x26 -> execSLAHL
              0x27 -> execSLA A
              0x28 -> execSRA B
              0x29 -> execSRA C
              0x2A -> execSRA D
              0x2B -> execSRA E
              0x2C -> execSRA H
              0x2D -> execSRA L
              0x2E -> execSRAHL
              0x2F -> execSRA A

              0x30 -> execSWAP B
              0x31 -> execSWAP C
              0x32 -> execSWAP D
              0x33 -> execSWAP E
              0x34 -> execSWAP H
              0x35 -> execSWAP L
              0x36 -> execSWAPHL
              0x37 -> execSWAP A
              0x38 -> execSRL B
              0x39 -> execSRL C
              0x3A -> execSRL D
              0x3B -> execSRL E
              0x3C -> execSRL H
              0x3D -> execSRL L
              0x3E -> execSRLHL
              0x3F -> execSRL A

              0x40 -> execBIT Bit0 B
              0x41 -> execBIT Bit0 C
              0x42 -> execBIT Bit0 D
              0x43 -> execBIT Bit0 E
              0x44 -> execBIT Bit0 H
              0x45 -> execBIT Bit0 L
              0x46 -> execBITHL Bit0
              0x47 -> execBIT Bit0 A
              0x48 -> execBIT Bit1 B
              0x49 -> execBIT Bit1 C
              0x4A -> execBIT Bit1 D
              0x4B -> execBIT Bit1 E
              0x4C -> execBIT Bit1 H
              0x4D -> execBIT Bit1 L
              0x4E -> execBITHL Bit1
              0x4F -> execBIT Bit1 A

              0x50 -> execBIT Bit2 B
              0x51 -> execBIT Bit2 C
              0x52 -> execBIT Bit2 D
              0x53 -> execBIT Bit2 E
              0x54 -> execBIT Bit2 H
              0x55 -> execBIT Bit2 L
              0x56 -> execBITHL Bit2
              0x57 -> execBIT Bit2 A
              0x58 -> execBIT Bit3 B
              0x59 -> execBIT Bit3 C
              0x5A -> execBIT Bit3 D
              0x5B -> execBIT Bit3 E
              0x5C -> execBIT Bit3 H
              0x5D -> execBIT Bit3 L
              0x5E -> execBITHL Bit3
              0x5F -> execBIT Bit3 A

              0x60 -> execBIT Bit4 B
              0x61 -> execBIT Bit4 C
              0x62 -> execBIT Bit4 D
              0x63 -> execBIT Bit4 E
              0x64 -> execBIT Bit4 H
              0x65 -> execBIT Bit4 L
              0x66 -> execBITHL Bit4
              0x67 -> execBIT Bit4 A
              0x68 -> execBIT Bit5 B
              0x69 -> execBIT Bit5 C
              0x6A -> execBIT Bit5 D
              0x6B -> execBIT Bit5 E
              0x6C -> execBIT Bit5 H
              0x6D -> execBIT Bit5 L
              0x6E -> execBITHL Bit5
              0x6F -> execBIT Bit5 A

              0x70 -> execBIT Bit6 B
              0x71 -> execBIT Bit6 C
              0x72 -> execBIT Bit6 D
              0x73 -> execBIT Bit6 E
              0x74 -> execBIT Bit6 H
              0x75 -> execBIT Bit6 L
              0x76 -> execBITHL Bit6
              0x77 -> execBIT Bit6 A
              0x78 -> execBIT Bit7 B
              0x79 -> execBIT Bit7 C
              0x7A -> execBIT Bit7 D
              0x7B -> execBIT Bit7 E
              0x7C -> execBIT Bit7 H
              0x7D -> execBIT Bit7 L
              0x7E -> execBITHL Bit7
              0x7F -> execBIT Bit7 A

              0x80 -> execRES Bit0 B
              0x81 -> execRES Bit0 C
              0x82 -> execRES Bit0 D
              0x83 -> execRES Bit0 E
              0x84 -> execRES Bit0 H
              0x85 -> execRES Bit0 L
              0x86 -> execRESHL Bit0
              0x87 -> execRES Bit0 A
              0x88 -> execRES Bit1 B
              0x89 -> execRES Bit1 C
              0x8A -> execRES Bit1 D
              0x8B -> execRES Bit1 E
              0x8C -> execRES Bit1 H
              0x8D -> execRES Bit1 L
              0x8E -> execRESHL Bit1
              0x8F -> execRES Bit1 A

              0x90 -> execRES Bit2 B
              0x91 -> execRES Bit2 C
              0x92 -> execRES Bit2 D
              0x93 -> execRES Bit2 E
              0x94 -> execRES Bit2 H
              0x95 -> execRES Bit2 L
              0x96 -> execRESHL Bit2
              0x97 -> execRES Bit2 A
              0x98 -> execRES Bit3 B
              0x99 -> execRES Bit3 C
              0x9A -> execRES Bit3 D
              0x9B -> execRES Bit3 E
              0x9C -> execRES Bit3 H
              0x9D -> execRES Bit3 L
              0x9E -> execRESHL Bit3
              0x9F -> execRES Bit3 A

              0xA0 -> execRES Bit4 B
              0xA1 -> execRES Bit4 C
              0xA2 -> execRES Bit4 D
              0xA3 -> execRES Bit4 E
              0xA4 -> execRES Bit4 H
              0xA5 -> execRES Bit4 L
              0xA6 -> execRESHL Bit4
              0xA7 -> execRES Bit4 A
              0xA8 -> execRES Bit5 B
              0xA9 -> execRES Bit5 C
              0xAA -> execRES Bit5 D
              0xAB -> execRES Bit5 E
              0xAC -> execRES Bit5 H
              0xAD -> execRES Bit5 L
              0xAE -> execRESHL Bit5
              0xAF -> execRES Bit5 A

              0xB0 -> execRES Bit6 B
              0xB1 -> execRES Bit6 C
              0xB2 -> execRES Bit6 D
              0xB3 -> execRES Bit6 E
              0xB4 -> execRES Bit6 H
              0xB5 -> execRES Bit6 L
              0xB6 -> execRESHL Bit6
              0xB7 -> execRES Bit6 A
              0xB8 -> execRES Bit7 B
              0xB9 -> execRES Bit7 C
              0xBA -> execRES Bit7 D
              0xBB -> execRES Bit7 E
              0xBC -> execRES Bit7 H
              0xBD -> execRES Bit7 L
              0xBE -> execRESHL Bit7
              0xBF -> execRES Bit7 A

              0xC0 -> execSET Bit0 B
              0xC1 -> execSET Bit0 C
              0xC2 -> execSET Bit0 D
              0xC3 -> execSET Bit0 E
              0xC4 -> execSET Bit0 H
              0xC5 -> execSET Bit0 L
              0xC6 -> execSETHL Bit0
              0xC7 -> execSET Bit0 A
              0xC8 -> execSET Bit1 B
              0xC9 -> execSET Bit1 C
              0xCA -> execSET Bit1 D
              0xCB -> execSET Bit1 E
              0xCC -> execSET Bit1 H
              0xCD -> execSET Bit1 L
              0xCE -> execSETHL Bit1
              0xCF -> execSET Bit1 A

              0xD0 -> execSET Bit2 B
              0xD1 -> execSET Bit2 C
              0xD2 -> execSET Bit2 D
              0xD3 -> execSET Bit2 E
              0xD4 -> execSET Bit2 H
              0xD5 -> execSET Bit2 L
              0xD6 -> execSETHL Bit2
              0xD7 -> execSET Bit2 A
              0xD8 -> execSET Bit3 B
              0xD9 -> execSET Bit3 C
              0xDA -> execSET Bit3 D
              0xDB -> execSET Bit3 E
              0xDC -> execSET Bit3 H
              0xDD -> execSET Bit3 L
              0xDE -> execSETHL Bit3
              0xDF -> execSET Bit3 A

              0xE0 -> execSET Bit4 B
              0xE1 -> execSET Bit4 C
              0xE2 -> execSET Bit4 D
              0xE3 -> execSET Bit4 E
              0xE4 -> execSET Bit4 H
              0xE5 -> execSET Bit4 L
              0xE6 -> execSETHL Bit4
              0xE7 -> execSET Bit4 A
              0xE8 -> execSET Bit5 B
              0xE9 -> execSET Bit5 C
              0xEA -> execSET Bit5 D
              0xEB -> execSET Bit5 E
              0xEC -> execSET Bit5 H
              0xED -> execSET Bit5 L
              0xEE -> execSETHL Bit5
              0xEF -> execSET Bit5 A

              0xF0 -> execSET Bit6 B
              0xF1 -> execSET Bit6 C
              0xF2 -> execSET Bit6 D
              0xF3 -> execSET Bit6 E
              0xF4 -> execSET Bit6 H
              0xF5 -> execSET Bit6 L
              0xF6 -> execSETHL Bit6
              0xF7 -> execSET Bit6 A
              0xF8 -> execSET Bit7 B
              0xF9 -> execSET Bit7 C
              0xFA -> execSET Bit7 D
              0xFB -> execSET Bit7 E
              0xFC -> execSET Bit7 H
              0xFD -> execSET Bit7 L
              0xFE -> execSETHL Bit7
              0xFF -> execSET Bit7 A

    0xCC -> execCALL (Just FlagZ) nn
    0xCD -> execCALL Nothing nn
    0xCE -> execADCN n
    0xCF -> execRST RST_08H

    0xD0 -> execRET (Just FlagNC)
    0xD1 -> execPOP StackRegDE
    0xD2 -> execJP (Just FlagNC) nn
    0xD3 -> error "$D3 Invalid Opcode"
    0xD4 -> execCALL (Just FlagNC) nn
    0xD5 -> execPUSH StackRegDE
    0xD6 -> execSUBN n
    0xD7 -> execRST RST_10H
    0xD8 -> execRET (Just FlagC)
    0xD9 -> execRETI
    0xDA -> execJP (Just FlagC) nn
    0xDB -> error "$DB Invalid Opcode"
    0xDC -> execCALL (Just FlagC) nn
    0xDD -> error "$DD Invalid Opcode"
    0xDE -> execSBCN n
    0xDF -> execRST RST_18H

    0xE0 -> execLDH True n
    0xE1 -> execPOP StackRegHL
    0xE2 -> execLDPC
    0xE3 -> error "$E3 Invalid Opcode"
    0xE4 -> error "$E4 Invalid Opcode"
    0xE5 -> execPUSH StackRegHL
    0xE6 -> execANDN n
    0xE7 -> execRST RST_20H
    0xE8 -> execADD2SP (fromIntegral n)
    0xE9 -> execJPHL
    0xEA -> execLDPN nn
    0xEB -> error "$EB Invalid Opcode"
    0xEC -> error "$EC Invalid Opcode"
    0xED -> error "$ED Invalid Opcode"
    0xEE -> execXORN n
    0xEF -> execRST RST_28H

    0xF0 -> execLDH False n
    0xF1 -> execPOP StackRegAF
    0xF2 -> execLDAC
    0xF3 -> execDI
    0xF4 -> error "$F4 Invalid Opcode"
    0xF5 -> execPUSH StackRegAF
    0xF6 -> execORN n
    0xF7 -> execRST RST_30H
    0xF8 -> execLDHL2 (fromIntegral n)
    0xF9 -> execLDSP2
    0xFA -> execLDAPN nn
    0xFB -> execEI
    0xFC -> error "$FC Invalid Opcode"
    0xFD -> error "$FD Invalid Opcode"
    0xFE -> execCPN n
    0xFF -> execRST RST_38H

  where
    regIORef r = case r of
      A -> msRegA s
      B -> msRegB s
      C -> msRegC s
      D -> msRegD s
      E -> msRegE s
      H -> msRegE s
      L -> msRegE s

    writeIORegister2 rp nn =
      let (hi, lo) = splitWord16 nn in
      case rp of
        BC -> writeIORef (msRegB s) hi >>
              writeIORef (msRegC s) lo
        DE -> writeIORef (msRegD s) hi >>
              writeIORef (msRegE s) lo
        HL -> writeIORef (msRegH s) hi >>
              writeIORef (msRegL s) lo
        SP -> writeIORef (msRegSP s) nn

    readIORegister2 rp =
      case rp of
        BC -> do hi <- readIORef (msRegB s)
                 lo <- readIORef (msRegC s)
                 return (joinWord16 hi lo)
        DE -> do hi <- readIORef (msRegD s)
                 lo <- readIORef (msRegE s)
                 return (joinWord16 hi lo)
        HL -> do hi <- readIORef (msRegH s)
                 lo <- readIORef (msRegL s)
                 return (joinWord16 hi lo)

    writeFlagsIO z n h c = do
      v0 <- readIORef (msRegF s)
      let v1 = case z of
                 Nothing -> v0
                 Just True -> setBit v0 7
                 Just False -> clearBit v0 7
      let v2 = case n of
                 Nothing -> v1
                 Just True -> setBit v1 6
                 Just False -> clearBit v1 6
      let v3 = case h of
                 Nothing -> v2
                 Just True -> setBit v2 5
                 Just False -> clearBit v2 5
      let v4 = case c of
                 Nothing -> v3
                 Just True -> setBit v3 4
                 Just False -> clearBit v3 4
      writeIORef (msRegF s) v4


    execLDR r1 r2 = do
      a <- readIORef (regIORef r1)
      writeIORef (regIORef r2) a
      incPC1
    execLDRN r n = do
      writeIORef (regIORef r) n
      incPC2
    execLDRHL r = do
      a <- readIORegister2 HL
      v <- readMemoryIO s a
      writeIORef (regIORef r) v
      incPC1
    execLDHL r = do
      a <- readIORegister2 HL 
      v <- readIORef (regIORef r)
      writeMemoryIO s a v
      incPC1
    execLDHLN n = do
      a <- readIORegister2 HL
      writeMemoryIO s a n
      incPC2
    execLDAP True = do
      a <- readIORegister2 BC
      v <- readMemoryIO s a
      writeIORef (msRegA s) v
      incPC1
    execLDAP False = do
      a <- readIORegister2 DE
      v <- readMemoryIO s a
      writeIORef (msRegA s) v
      incPC1
    execLDAPN nn = do
      v <- readMemoryIO s nn
      writeIORef (msRegA s) v
      incPC3
    execLDAC = do
      o <- readIORef (msRegC s)
      let a = 0xFF00 + (fromIntegral o)
      v <- readMemoryIO s a
      writeIORef (msRegA s) v
      incPC1
    execLDPR True = do
      a <- readIORegister2 BC
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      incPC1
    execLDPR False = do
      a <- readIORegister2 DE
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      incPC1
    execLDPN a = do
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      incPC3
    execLDPC = do
      o <- readIORef (msRegC s)
      let a = 0xFF00 + (fromIntegral o)::Word16
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      incPC1
    execLD2 rp nn = do
      writeIORegister2 rp nn
      incPC3
    execLDSP2 = do
      v <- readIORegister2 HL
      writeIORegister2 SP v
      incPC1
    execLDP2 nn = do
      v <- readIORegister2 SP
      let (v', v'') = splitWord16 v
      let nn' = nn + 1
      writeMemoryIO s nn v'
      writeMemoryIO s nn' v''
      incPC3
    execLDI True = do
      a <- readIORegister2 HL
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      let a' = a + 1
      writeIORegister2 HL a'
      incPC1
    execLDI False = do
      a <- readIORegister2 HL
      v <- readMemoryIO s a
      writeIORef (msRegA s) v
      let a' = a + 1
      writeIORegister2 HL a'
      incPC1
    execLDD True = do
      a <- readIORegister2 HL
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      let a' = a - 1
      writeIORegister2 HL a'
      incPC1
    execLDD False = do
      a <- readIORegister2 HL
      v <- readMemoryIO s a
      writeIORef (msRegA s) v
      let a' = a - 1
      writeIORegister2 HL a'
      incPC1
    execLDH True n = do
      let a = 0xFF00 + (fromIntegral n)
      v <- readIORef (msRegA s)
      writeMemoryIO s a v
      incPC2
    execLDH False n = do
      let a = 0xFF00 + (fromIntegral n)
      v <- readMemoryIO s a
      writeIORef (msRegA s) v
      incPC2
    execLDHL2 d = do
      v <- readIORegister2 SP
      let v' = v + (fromIntegral d)
      writeIORegister2 HL v'
      writeFlagsIO (Just False) (Just False) (Just (v'.&.0x000F < v.&.0x000F)) (Just (v' < v))
      incPC2
    execPUSH :: StackRegister -> IO ()
    execPUSH sr = do
      a <- readIORegister2 SP
      let a' = a - 1
      let a'' = a - 2
      (hi, lo) <- case sr of
                    StackRegAF -> do hi <- readIORef (msRegA s)
                                     lo <- readIORef (msRegF s)
                                     return (hi, lo)
                    StackRegBC -> do hi <- readIORef (msRegB s)
                                     lo <- readIORef (msRegC s)
                                     return (hi, lo)
                    StackRegDE -> do hi <- readIORef (msRegD s)
                                     lo <- readIORef (msRegE s)
                                     return (hi, lo)
                    StackRegHL -> do hi <- readIORef (msRegH s)
                                     lo <- readIORef (msRegL s)
                                     return (hi, lo)
      writeMemoryIO s a' hi
      writeMemoryIO s a'' lo
      writeIORegister2 SP a''
      incPC1
    execPOP :: StackRegister -> IO ()
    execPOP sr = do
      a <- readIORegister2 SP
      let a' = a + 1
      lo <- readMemoryIO s a
      hi <- readMemoryIO s a'
      case sr of
        StackRegAF -> writeIORef (msRegA s) hi >>
                      writeIORef (msRegF s) lo
        StackRegBC -> writeIORef (msRegB s) hi >>
                      writeIORef (msRegC s) lo
        StackRegDE -> writeIORef (msRegD s) hi >>
                      writeIORef (msRegE s) lo
        StackRegHL -> writeIORef (msRegH s) hi >>
                      writeIORef (msRegL s) lo
      let a'' = a + 2
      writeIORegister2 SP a''
      incPC1
    execADD r = return ()
    execADDN n = return ()
    execADDHL = return ()
    execADC r = return ()
    execADCN n = return ()
    execADCHL = return ()
    execSUB r = return ()
    execSUBN n = return ()
    execSUBHL = return ()
    execSBC r = return ()
    execSBCN n = return ()
    execSBCHL = return ()
    execAND r = return ()
    execANDN n = return ()
    execANDHL = return ()
    execOR r = return ()
    execORN n = return ()
    execORHL = return ()
    execXOR r = return ()
    execXORN n = return ()
    execXORHL = return ()
    execCP r = return ()
    execCPN n = return ()
    execCPHL = return ()
    execINC r = return ()
    execINC2 rp = return ()
    execINCHL = return ()
    execDEC r = return ()
    execDEC2 rp = return ()
    execDECHL = return ()
    execADD2HL rp = return ()
    execADD2SP n = return ()
    execSWAP r = return ()
    execSWAPHL = return ()
    execDAA = error "DAA | NOT IMPLEMENTED"
    execCPL = return ()
    execCCF = return ()
    execSCF = return ()
    execNOP = incPC1
    execHALT = incPC2
    execSTOP = incPC2
    execDI = incPC1
    execEI = incPC1
    execRLCA = return ()
    execRLA = return ()
    execRRCA = return ()
    execRRA = return ()
    execRLC r = return ()
    execRLCHL = return ()
    execRL r = return ()
    execRLHL = return ()
    execRRC r = return ()
    execRRCHL = return ()
    execRR r = return ()
    execRRHL = return ()
    execSLA r = return ()
    execSLAHL = return ()
    execSRA r = error "SRA r | NOT IMPLEMENTED"
    execSRAHL = error "SRA (HL) | NOT IMPLEMENTED"
    execSRL r = return ()
    execSRLHL = return ()
    execBIT bi r = return ()
    execBITHL bi = return ()
    execSET bi r = return ()
    execSETHL bi = return ()
    execRES bi r = return ()
    execRESHL bi = return ()
    execJP Nothing nn = return ()
    execJP (Just cc) nn = return ()
    execJPHL = return ()
    execJR Nothing n = return ()
    execJR (Just cc) n = return ()
    execCALL Nothing nn = return ()
    execCALL (Just cc) nn = return ()
    execRST ra = return ()
    execRET Nothing = return ()
    execRET (Just cc) = return ()
    execRETI = return ()

    incPC1 = modifyIORef (msRegPC s) (+1)
    incPC2 = modifyIORef (msRegPC s) (+2)
    incPC3 = modifyIORef (msRegPC s) (+3)

