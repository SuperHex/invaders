-- | Disassembler for Intel 8080 CPU
{-# LANGUAGE OverloadedStrings #-}

module Disassembly where

import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString     as B
import qualified Data.Vector         as V
import           Data.Word
import           Utils               (decode, encode)

data Reg
  = A
  | B
  | C
  | D
  | E
  | H
  | L
  | M
  | SP
  | PSW
  deriving (Show,Eq)

data Instruction
  = NOP
  | LXI Reg Word8 Word8
  | STAX Reg
  | INX Reg
  | INR Reg
  | DCR Reg
  | MVI Reg Word8
  | RLC
  | RRC
  | DAD Reg
  | LDA Addr
  | LDAX Reg
  | DCX Reg
  | RAL
  | RAR
  | RIM
  | SHLD Addr
  | DAA
  | LHLD Addr
  | CMA
  | SIM
  | STA Addr
  | STC
  | CMC
  | MOV Reg Reg
  | HLT
  | ADD Reg
  | ADC Reg
  | SUB Reg
  | SBB Reg
  | ANA Reg
  | XRA Reg
  | ORA Reg
  | CMP Reg
  | RNZ
  | POP Reg
  | JNZ Addr
  | JMP Addr
  | CNZ Addr
  | PUSH Reg
  | ADI Word8
  | RST Int
  | RZ
  | RET
  | JZ Addr
  | CZ Addr
  | CALL Addr
  | ACI Word8
  | RNC
  | JNC Addr
  | OUT Word8
  | CNC Addr
  | SUI Word8
  | RC
  | JC Addr
  | IN Word8
  | CC Addr
  | SBI Word8
  | RPO
  | JPO Addr
  | XTHL
  | CPO Addr
  | ANI Word8
  | RPE
  | PCHL
  | JPE Addr
  | XCHG
  | CPE Addr
  | XRI Word8
  | RP
  | JP Addr
  | DI
  | CP Addr
  | ORI Word8
  | RM
  | SPHL
  | JM Addr
  | EI
  | CM Addr
  | CPI Word8
  | DATA Word8
  deriving (Show, Eq)

type Addr = Word16
type Program = [Instruction]
type Raw = V.Vector Word8
type ROM = V.Vector Instruction

data Parser a = Parser
  { parsed   :: a
  , unparsed :: a }

type ParseState a = StateT (Parser B.ByteString) IO a

readHexFile :: FilePath -> IO (Parser B.ByteString)
readHexFile path = do
  hex <- B.readFile path
  return $ Parser B.empty hex

readByte :: ParseState (Maybe Word8)
readByte = do
  p <- get
  let byte = B.take 1 (unparsed p)
  put $ p { parsed = parsed p `B.append` byte
          , unparsed = B.drop 1 (unparsed p) }
  case B.unpack byte of
    [x] -> return (Just x)
    _   -> return Nothing

readAddr :: ParseState Addr
readAddr = do
  Just x <- readByte
  Just y <- readByte
  return $ encode y x

instruction :: ParseState Program
instruction = do
  byte <- readByte
  -- liftIO . print $ show byte ++ "\n"
  case byte of
    Nothing -> return []
    Just b ->
      case b of
        0x00 -> NOP <:> instruction
        0x01 -> do Just x <- readByte
                   Just y <- readByte
                   LXI B x y
                     <:> DATA x
                     <:> DATA y
                     <:> instruction
        0x02 -> STAX B <:> instruction
        0x03 -> INX B <:> instruction
        0x04 -> INR B <:> instruction
        0x05 -> DCR B <:> instruction
        0x06 -> do Just x <- readByte
                   MVI B x
                     <:> DATA x
                     <:> instruction
        0x07 -> RLC <:> instruction
        0x08 -> DATA 0x00
                <:> instruction -- EVil !!!
        0x09 -> DAD B <:> instruction
        0x0A -> LDAX B <:> instruction
        0x0B -> DCX B <:> instruction
        0x0C -> INR C <:> instruction
        0x0D -> DCR C <:> instruction
        0x0E -> do Just x <- readByte
                   MVI C x
                     <:> DATA x
                     <:> instruction
        0x0F -> RRC <:> instruction
        0x10 -> DATA 0x00
                <:> instruction -- Evil !
        0x11 -> do Just x <- readByte
                   Just y <- readByte
                   LXI D x y
                     <:> DATA x
                     <:> DATA y
                     <:> instruction
        0x12 -> STAX D <:> instruction
        0x13 -> INX D <:> instruction
        0x14 -> INR D <:> instruction
        0x15 -> DCR D <:> instruction
        0x16 -> do Just x <- readByte
                   MVI D x
                     <:> DATA x
                     <:> instruction
        0x17 -> RAL <:> instruction
        0x18 -> DATA 0x00
                <:> instruction -- Evil!
        0x19 -> DAD D <:> instruction
        0x1A -> LDAX D <:> instruction
        0x1B -> DCX D <:> instruction
        0x1C -> INR E <:> instruction
        0x1D -> DCR E <:> instruction
        0x1E -> do Just x <- readByte
                   MVI E x
                     <:> DATA x
                     <:> instruction
        0x1F -> RAR <:> instruction
        0x20 -> RIM <:> instruction
        0x21 -> do Just x <- readByte
                   Just y <- readByte
                   LXI H x y
                     <:> DATA x
                     <:> DATA y
                     <:> instruction
        0x22 -> do x <- readAddr
                   let (hi,lo) = decode x
                   SHLD x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction  -- little endian
        0x23 -> INX H <:> instruction
        0x24 -> INR H <:> instruction
        0x25 -> DCR H <:> instruction
        0x26 -> do Just x <- readByte
                   MVI H x
                     <:> DATA x
                     <:> instruction
        0x27 -> DAA <:> instruction
        0x28 -> DATA 0x00
                <:> instruction -- Evil!
        0x29 -> DAD H <:> instruction
        0x2A -> do x <- readAddr
                   let (hi,lo) = decode x
                   LHLD x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0x2B -> DCX H <:> instruction
        0x2C -> INR L <:> instruction
        0x2D -> DCR L <:> instruction
        0x2E -> do Just x <- readByte
                   MVI L x
                     <:> DATA x
                     <:> instruction
        0x2F -> CMA <:> instruction
        0x30 -> SIM <:> instruction
        0x31 -> do Just x <- readByte
                   Just y <- readByte
                   LXI SP x y
                     <:> DATA x
                     <:> DATA y
                     <:> instruction
        0x32 -> do x <- readAddr
                   let (hi,lo) = decode x
                   STA x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0x33 -> INX SP <:> instruction
        0x34 -> INR M <:> instruction
        0x35 -> DCR M <:> instruction
        0x36 -> do Just x <- readByte
                   MVI M x
                     <:> DATA x
                     <:> instruction
        0x37 -> STC <:> instruction
        0x38 -> DATA 0x00
                <:> instruction
        0x39 -> DAD SP <:> instruction
        0x3A -> do x <- readAddr
                   let (hi,lo) = decode x
                   LDA x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0x3B -> DCX SP <:> instruction
        0x3C -> INR A <:> instruction
        0x3D -> DCR A <:> instruction
        0x3E -> do (Just x) <- readByte
                   MVI A x
                     <:> DATA x
                     <:> instruction
        0x3F -> CMC <:> instruction
        0x40 -> MOV B B <:> instruction
        0x41 -> MOV B C <:> instruction
        0x42 -> MOV B D <:> instruction
        0x43 -> MOV B E <:> instruction
        0x44 -> MOV B H <:> instruction
        0x45 -> MOV B L <:> instruction
        0x46 -> MOV B M <:> instruction
        0x47 -> MOV B A <:> instruction
        0x48 -> MOV C B <:> instruction
        0x49 -> MOV C C <:> instruction
        0x4A -> MOV C D <:> instruction
        0x4B -> MOV C E <:> instruction
        0x4C -> MOV C H <:> instruction
        0x4D -> MOV C L <:> instruction
        0x4E -> MOV C M <:> instruction
        0x4F -> MOV C A <:> instruction
        0x50 -> MOV D B <:> instruction
        0x51 -> MOV D C <:> instruction
        0x52 -> MOV D D <:> instruction
        0x53 -> MOV D E <:> instruction
        0x54 -> MOV D H <:> instruction
        0x55 -> MOV D L <:> instruction
        0x56 -> MOV D M <:> instruction
        0x57 -> MOV D A <:> instruction
        0x58 -> MOV E B <:> instruction
        0x59 -> MOV E C <:> instruction
        0x5A -> MOV E D <:> instruction
        0x5B -> MOV E E <:> instruction
        0x5C -> MOV E H <:> instruction
        0x5D -> MOV E L <:> instruction
        0x5E -> MOV E M <:> instruction
        0x5F -> MOV E A <:> instruction
        0x60 -> MOV H B <:> instruction
        0x61 -> MOV H C <:> instruction
        0x62 -> MOV H D <:> instruction
        0x63 -> MOV H E <:> instruction
        0x64 -> MOV H H <:> instruction
        0x65 -> MOV H L <:> instruction
        0x66 -> MOV H M <:> instruction
        0x67 -> MOV H A <:> instruction
        0x68 -> MOV L B <:> instruction
        0x69 -> MOV L C <:> instruction
        0x6A -> MOV L D <:> instruction
        0x6B -> MOV L E <:> instruction
        0x6C -> MOV L H <:> instruction
        0x6D -> MOV L L <:> instruction
        0x6E -> MOV L M <:> instruction
        0x6F -> MOV L A <:> instruction
        0x70 -> MOV M B <:> instruction
        0x71 -> MOV M C <:> instruction
        0x72 -> MOV M D <:> instruction
        0x73 -> MOV M E <:> instruction
        0x74 -> MOV M H <:> instruction
        0x75 -> MOV M L <:> instruction
        0x76 -> HLT <:> instruction
        0x77 -> MOV M A <:> instruction
        0x78 -> MOV A B <:> instruction
        0x79 -> MOV A C <:> instruction
        0x7A -> MOV A D <:> instruction
        0x7B -> MOV A E <:> instruction
        0x7C -> MOV A H <:> instruction
        0x7D -> MOV A L <:> instruction
        0x7E -> MOV A M <:> instruction
        0x7F -> MOV A A <:> instruction
        0x80 -> ADD B <:> instruction
        0x81 -> ADD C <:> instruction
        0x82 -> ADD D <:> instruction
        0x83 -> ADD E <:> instruction
        0x84 -> ADD H <:> instruction
        0x85 -> ADD L <:> instruction
        0x86 -> ADD M <:> instruction
        0x87 -> ADD A <:> instruction
        0x88 -> ADC B <:> instruction
        0x89 -> ADC C <:> instruction
        0x8A -> ADC D <:> instruction
        0x8B -> ADC E <:> instruction
        0x8C -> ADC H <:> instruction
        0x8D -> ADC L <:> instruction
        0x8E -> ADC M <:> instruction
        0x8F -> ADC A <:> instruction
        0x90 -> SUB B <:> instruction
        0x91 -> SUB C <:> instruction
        0x92 -> SUB D <:> instruction
        0x93 -> SUB E <:> instruction
        0x94 -> SUB H <:> instruction
        0x95 -> SUB L <:> instruction
        0x96 -> SUB M <:> instruction
        0x97 -> SUB A <:> instruction
        0x98 -> SBB B <:> instruction
        0x99 -> SBB C <:> instruction
        0x9A -> SBB D <:> instruction
        0x9B -> SBB E <:> instruction
        0x9C -> SBB H <:> instruction
        0x9D -> SBB L <:> instruction
        0x9E -> SBB M <:> instruction
        0x9F -> SBB A <:> instruction
        0xA0 -> ANA B <:> instruction
        0xA1 -> ANA C <:> instruction
        0xA2 -> ANA D <:> instruction
        0xA3 -> ANA E <:> instruction
        0xA4 -> ANA H <:> instruction
        0xA5 -> ANA L <:> instruction
        0xA6 -> ANA M <:> instruction
        0xA7 -> ANA A <:> instruction
        0xA8 -> XRA B <:> instruction
        0xA9 -> XRA C <:> instruction
        0xAA -> XRA D <:> instruction
        0xAB -> XRA E <:> instruction
        0xAC -> XRA H <:> instruction
        0xAD -> XRA L <:> instruction
        0xAE -> XRA M <:> instruction
        0xAF -> XRA A <:> instruction
        0xB0 -> ORA B <:> instruction
        0xB1 -> ORA C <:> instruction
        0xB2 -> ORA D <:> instruction
        0xB3 -> ORA E <:> instruction
        0xB4 -> ORA H <:> instruction
        0xB5 -> ORA L <:> instruction
        0xB6 -> ORA M <:> instruction
        0xB7 -> ORA A <:> instruction
        0xB8 -> CMP B <:> instruction
        0xB9 -> CMP C <:> instruction
        0xBA -> CMP D <:> instruction
        0xBB -> CMP E <:> instruction
        0xBC -> CMP H <:> instruction
        0xBD -> CMP L <:> instruction
        0xBE -> CMP M <:> instruction
        0xBF -> CMP A <:> instruction
        0xC0 -> RNZ <:> instruction
        0xC1 -> POP B <:> instruction
        0xC2 -> do x <- readAddr
                   let (hi,lo) = decode x
                   JNZ x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xC3 -> do x <- readAddr
                   let (hi,lo) = decode x
                   JMP x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xC4 -> do x <- readAddr
                   let (hi,lo) = decode x
                   CNZ x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xC5 -> PUSH B <:> instruction
        0xC6 -> do Just x <- readByte
                   ADI x
                     <:> DATA x
                     <:> instruction
        0xC7 -> RST 0 <:> instruction
        0xC8 -> RZ <:> instruction
        0xC9 -> RET <:> instruction
        0xCA -> do x <- readAddr
                   let (hi,lo) = decode x
                   JZ x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xCB -> DATA 0x00 <:> instruction  -- Evil
        0xCC -> do x <- readAddr
                   let (hi,lo) = decode x
                   CZ x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xCD -> do x <- readAddr
                   let (hi,lo) = decode x
                   CALL x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xCE -> do Just x <- readByte
                   ACI x
                     <:> DATA x
                     <:> instruction
        0xCF -> RST 1 <:> instruction
        0xD0 -> RNC <:> instruction
        0xD1 -> POP D <:> instruction
        0xD2 -> do x <- readAddr
                   let (hi,lo) = decode x
                   JNC x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xD3 -> do Just x <- readByte
                   OUT x
                     <:> DATA x
                     <:> instruction
        0xD4 -> do x <- readAddr
                   let (hi,lo) = decode x
                   CNC x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xD5 -> PUSH D <:> instruction
        0xD6 -> do Just x <- readByte
                   SUI x
                     <:> DATA x
                     <:> instruction
        0xD7 -> RST 2 <:> instruction
        0xD8 -> RC <:> instruction
        0xDA -> do x <- readAddr
                   let (hi,lo) = decode x
                   JC x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xDB -> do Just x <- readByte
                   IN x
                     <:> DATA x
                     <:> instruction
        0xDC -> do x <- readAddr
                   let (hi,lo) = decode x
                   CC x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xDE -> do Just x <- readByte
                   SBI x
                     <:> DATA x
                     <:> instruction
        0xDF -> RST 3 <:> instruction
        0xE0 -> RPO <:> instruction
        0xE1 -> POP H <:> instruction
        0xE2 -> do x <- readAddr
                   let (hi,lo) = decode x
                   JPO x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xE3 -> XTHL <:> instruction
        0xE4 -> do x <- readAddr
                   let (hi,lo) = decode x
                   CPO x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xE5 -> PUSH H <:> instruction
        0xE6 -> do Just x <- readByte
                   ANI x
                     <:> DATA x
                     <:> instruction
        0xE7 -> RST 4 <:> instruction
        0xE8 -> RPE <:> instruction
        0xE9 -> PCHL <:> instruction
        0xEA -> do x <- readAddr
                   let (hi,lo) = decode x
                   JPE x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xEB -> XCHG <:> instruction
        0xEC -> do x <- readAddr
                   let (hi,lo) = decode x
                   CPE x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xED -> DATA 0x00
                <:> instruction
        0xEE -> do Just x <- readByte
                   XRI x
                     <:> DATA x
                     <:> instruction
        0xEF -> RST 5 <:> instruction
        0xF0 -> RP <:> instruction
        0xF1 -> POP PSW <:> instruction
        0xF2 -> do x <- readAddr
                   let (hi,lo) = decode x
                   JP x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xF3 -> DI <:> instruction
        0xF4 -> do x <- readAddr
                   let (hi,lo) = decode x
                   CP x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xF5 -> PUSH PSW <:> instruction
        0xF6 -> do Just x <- readByte
                   ORI x
                     <:> DATA x
                     <:> instruction
        0xF7 -> RST 6 <:> instruction
        0xF8 -> RM <:> instruction
        0xF9 -> SPHL <:> instruction
        0xFA -> do x <- readAddr
                   let (hi,lo) = decode x
                   JM x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xFB -> EI <:> instruction
        0xFC -> do x <- readAddr
                   let (hi,lo) = decode x
                   CM x
                     <:> DATA lo
                     <:> DATA hi
                     <:> instruction
        0xFE -> do Just x <- readByte
                   CPI x
                     <:> DATA x
                     <:> instruction
        0xFF -> RST 7 <:> instruction
        ins    -> error $ "undefined instruction:" ++ show ins

infixr 5 <:>
(<:>) :: (Functor f) => a -> f [a] -> f [a]
x <:> y = fmap (x:) y

(<++>) :: (Functor f) => [a] -> f [a] -> f [a]
x <++> y = fmap (x ++) y

raw :: FilePath -> IO Raw
raw path = do
  hex <- B.readFile path
  return $ V.fromList (B.unpack hex)

parse :: FilePath -> IO ROM
parse path = do
  p <- readHexFile path
  V.fromList <$> evalStateT instruction p

