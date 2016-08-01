{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Emulator where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits
import qualified Data.Vector          as V
import           Data.Vector.Lens
import           Data.Word
import           Disassembly
import           Numeric
import           Utils

data Flags a = Flags
  { _z   ::  a
  , _s   ::  a
  , _p   ::  a
  , _cy  ::  a
  , _ac  ::  a
  , _pad ::  a
  } deriving (Show)

data CPUState a b = CPU
  { _regA      ::  a
  , _regB      ::  a
  , _regC      ::  a
  , _regD      ::  a
  , _regE      ::  a
  , _regH      ::  a
  , _regL      ::  a
  , _spl       ::  a
  , _sph       ::  a
  , _pc        ::  b
  , _memory    ::  V.Vector a
  , _flags     ::  Flags a
  , _interrupt ::  Bool
  , _ports     ::  V.Vector a
  } deriving (Show)

makeLenses ''Flags
makeLenses ''CPUState

type CPU a = ReaderT Raw (ReaderT ROM (StateT (CPUState Word8 Word16) IO)) a

data CondFlag = NOCy
              | Cy
              | All
              deriving (Show, Eq)

newCPU :: CPUState Word8 Word16
newCPU =
  CPU { _regA = 0x00
      , _regB = 0x00
      , _regC = 0x00
      , _regD = 0x00
      , _regE = 0x00
      , _regH = 0x00
      , _regL = 0x00
      , _spl  = 0xFF
      , _sph  = 0x23
      , _pc   = 0x0000
      , _memory = V.replicate 0x4000 0x00
      , _ports =  V.replicate 0x100 0x00
      , _flags = Flags
          { _z = 0x00
          , _s = 0x00
          , _p = 0x00
          , _cy    = 0x00
          , _ac    = 0x00
          , _pad   = 0x00 }
      , _interrupt = True }

withFlags :: (forall a. (Bits a, Num a) => a -> a -> a)
          -> CondFlag
          -> Word8 -> Word8
          -> CPU Word8
withFlags f NOCy a b = do
  if f a b == 0x00
    then flags.z .= 1
    else flags.z .= 0
  if popCount (f a b) `mod` 2 == 0
    then flags.p .= 1
    else flags.p .= 0
  if a .&. 0x0F `f` b .&. 0x0F > 0x0F
    then flags.ac .= 1
    else flags.ac .= 0
  if f a b .&. 0x80 == 0x80
    then flags.s .= 1
    else flags.s .= 0
  return $ f a b
withFlags f Cy a b = do
  let a' = fromIntegral a :: Word16
      b' = fromIntegral b :: Word16
  if f a' b' > 0xFF
    then flags.cy .= 1
    else flags.cy .= 0
  return $ f a b
withFlags f All a b = do
  let a' = fromIntegral a :: Word16
      b' = fromIntegral b :: Word16
  if f a' b' > 0xFF
    then flags.cy .= 1
    else flags.cy .= 0
  if f a b == 0x00
    then flags.z .= 1
    else flags.z .= 0
  if popCount (f a b) `mod` 2 == 0
    then flags.p .= 1
    else flags.p .= 0
  if a .&. 0x0F `f` b .&. 0x0F > 0x0F
    then flags.ac .= 1
    else flags.ac .= 0
  if f a b .&. 0x80 == 0x80
    then flags.s .= 1
    else flags.s .= 0
  return $ f a b

fromReg :: Reg -> Lens (CPUState a b) (CPUState a b) a a
fromReg A  = regA
fromReg B  = regB
fromReg C  = regC
fromReg D  = regD
fromReg E  = regE
fromReg H  = regH
fromReg L  = regL
fromReg SP = sph

succReg :: Reg -> Lens (CPUState a b) (CPUState a b) a a
succReg B  = fromReg C
succReg D  = fromReg E
succReg H  = fromReg L
succReg SP = spl
succReg _  = error "not a register pair!"

runInstr :: Instruction -> CPU ()
runInstr (DATA _) = error "instruction cannot be data!"
runInstr NOP = pc += 1
runInstr (LXI reg lsb msb) = do
                                fromReg reg .= msb
                                succReg reg .= lsb
                                pc   += 3
runInstr (STAX reg) = do cpu <- get
                         memory %= \x -> V.update x
                                        ([(encode (cpu ^. fromReg reg) (cpu ^. succReg reg), cpu ^. regA)] ^. vector)
                         pc += 1
runInstr (INX reg) = do cpu <- get
                        let (x,y) = decode . (+1) $ encode (cpu ^. fromReg reg) (cpu ^. succReg reg)
                        fromReg reg .= x
                        succReg reg .= y
                        pc += 1
runInstr (INR M)   = do cpu <- get
                        let hl = encode (cpu ^. regH) (cpu ^. regL)
                        hl' <- withFlags (+) NOCy ((cpu ^. memory) V.! hl) 1
                        memory %= \x -> V.update x ([(hl, hl')] ^. vector)
                        pc += 1
runInstr (INR reg) = do cpu <- get
                        withFlags (+) NOCy (cpu ^. fromReg reg) (1 :: Word8) >>= \x -> fromReg reg .= x
                        pc += 1
runInstr (DCR M)   = do cpu <- get
                        let hl = encode (cpu ^. regH) (cpu ^. regL)
                        hl' <- withFlags (+) NOCy ((cpu ^. memory) V.! hl) (-1)
                        memory %= \x -> V.update x ([(hl, hl')] ^. vector)
                        pc += 1
runInstr (DCR reg) = do cpu <- get
                        b' <- withFlags (+) NOCy (cpu ^. fromReg reg) (-1 :: Word8)
                        fromReg reg .= b'
                        pc += 1
runInstr (MVI M d)   = do cpu <- get
                          let hl = encode (cpu ^. regH) (cpu ^. regL)
                          if hl .&. 0x8000 /= 0
                            then error "IO ports are not implemented yet"
                            else
                              memory %= \x -> V.update x ([(hl, d)] ^. vector)
                          pc += 2
runInstr (MVI reg d) = do fromReg reg .= d
                          pc += 2
runInstr RLC = do cpu <- get
                  let bits = cpu ^. regA .&. 0x80
                  regA %= \x -> x `shiftL` 1 .|. (bits `shiftR` 7)
                  flags.cy .= bits `shiftR` 7
                  pc += 1
runInstr (DAD reg) = do cpu <- get
                        let hl = encode (cpu ^. regH) (cpu ^. regL) :: Word32
                            xy = encode (cpu ^. fromReg reg) (cpu ^. succReg reg) :: Word32
                            ans = hl + xy
                        when (ans > 0xFFFF) (flags.cy .= 1)
                        let (h,l) = decode . fromIntegral $ ans
                        regH .= h
                        regL .= l
                        pc += 1
runInstr (LDAX reg) = do cpu <- get
                         let addr = encode (cpu ^. fromReg reg) (cpu ^. succReg reg)
                         if addr .&. 0x8000 /= 0
                           then do rom <- ask
                                   regA .= rom V.! addr
                           else
                             regA .= (cpu ^. memory) V.! addr
                         pc += 1
runInstr (DCX reg) = do cpu <- get
                        let (b', c') = decode . (\x -> x-1) $
                                       encode (cpu ^. fromReg reg) (cpu ^. succReg reg)
                        fromReg reg .= b'
                        succReg reg .= c'
                        pc += 1
runInstr RRC = do cpu <- get
                  let bits = cpu ^. regA .&. 0x01
                  regA %= \x -> x `shiftR` 1 .|. (bits `shiftL` 7)
                  flags.cy .= bits
                  pc += 1
runInstr RAL = do cpu <- get
                  let c = cpu ^. flags.cy
                      bits = cpu ^. regA .&. 0x80
                  regA %= \x -> x `shiftL` 1 .|. c
                  flags.cy .= bits `shiftR` 7
                  pc += 1
runInstr RAR = do cpu <- get
                  let c = cpu ^. flags.cy
                      bits = cpu ^.regA .&. 0x01
                  regA %= \x -> x `shiftR` 1 .|. (c `shiftL` 7)
                  flags.cy .= bits
                  pc += 1
runInstr RIM = undefined
-- TODO : implemetn RIM
runInstr (SHLD addr) = do cpu <- get
                          if addr .&. 0x8000 /= 0
                            then error "IO ports are not implement yet"
                            else memory %= \x -> V.update x ([ (fromIntegral addr, cpu ^. regL)
                                                            , (fromIntegral addr + 1, cpu ^. regH)
                                                            ] ^. vector)
                          pc += 3
runInstr DAA = undefined
-- TODO : implement DAA
runInstr (LHLD addr) = do cpu <- get
                          if addr .&. 0x8000 /= 0
                            then do
                                   rom <- ask
                                   regL .= rom V.! fromIntegral addr
                                   regH .= rom V.! fromIntegral (addr + 1)
                            else do
                                   regL .= (cpu ^. memory) V.! fromIntegral addr
                                   regH .= (cpu ^. memory) V.! fromIntegral (addr + 1)
                          pc += 3
runInstr CMA = do regA %= xor 0xff
                  pc += 1
runInstr SIM = undefined
-- TODO : implement SIM
runInstr (STA addr) = do cpu <- get
                         if addr .&. 0x8000 /= 0
                           then error "IO ports are not implemented yet"
                           else memory %= \x -> V.update x ([(fromIntegral addr, cpu ^. regA)] ^. vector)
                         pc += 3
runInstr STC = do flags.cy .= 1
                  pc += 1
runInstr (LDA addr) = do cpu <- get
                         if addr .&. 0x8000 /= 0
                           then do
                                  rom <- ask
                                  regA .= rom V.! fromIntegral addr
                           else regA .= (cpu ^. memory) V.! fromIntegral addr
                         pc += 3
runInstr CMC = do flags.cy %= \x -> x `xor` bit 0
                  pc += 1
runInstr (MOV t M) = do cpu <- get
                        let addr = encode (cpu ^. regH) (cpu ^. regL)
                        if addr .&. 0x8000 /= 0
                          then do
                                 rom <- ask
                                 fromReg t .= rom V.! addr
                          else fromReg t .= (cpu ^. memory) V.! addr
                        pc += 1
runInstr (MOV M f) = do cpu <- get
                        let addr = encode (cpu ^. regH) (cpu ^. regL)
                        if addr .&. 0x8000 /= 0
                          then error "IO ports are not implemented yet"
                          else memory %= \x -> V.update x ([(addr, cpu ^. fromReg f)] ^. vector)
                        pc += 1
runInstr (MOV t f) = do cpu <- get
                        fromReg t .= cpu ^. fromReg f
                        pc += 1
runInstr HLT = undefined
-- TODO : implement HLT
runInstr (ADD M) = do cpu <- get
                      let addr = encode (cpu ^. regH) (cpu ^. regL)
                      if addr .&. 0x8000 /= 0
                        then do
                               rom <- ask
                               r <- withFlags (+) All (cpu ^. regA) (rom V.! addr)
                               regA .= r
                        else do
                               r <- withFlags (+) All (cpu ^. regA) ((cpu ^. memory) V.! addr)
                               regA .= r
                      pc += 1
runInstr (ADD x) = do cpu <- get
                      r <- withFlags (+) All (cpu ^. regA) (cpu ^. fromReg x)
                      regA .= r
                      pc += 1
runInstr (ADC M) = do cpu <- get
                      rom <- ask
                      let addr = encode (cpu ^. regH) (cpu ^. regL)
                          mem = (cpu ^. memory) V.! addr
                          val = rom V.! addr
                      r  <- withFlags (+) All (cpu ^. flags.cy) (if addr .&. 0x8000 /= 0 then val else mem)
                      r' <- withFlags (+) All (cpu ^. regA) r
                      regA .= r'
                      pc += 1
runInstr (ADC reg) = do cpu <- get
                        r  <- withFlags (+) All (cpu ^. flags.cy) (cpu ^. fromReg reg)
                        r' <- withFlags (+) All (cpu ^. regA) r
                        regA .= r'
                        pc += 1
runInstr (SUB M) = do cpu <- get
                      rom <- ask
                      let addr = encode (cpu ^. regH) (cpu ^. regL)
                          v = if addr .&. 0x8000 == 0
                            then (cpu ^. memory) V.! addr
                            else rom V.! addr
                      r <- withFlags (-) All (cpu ^. regA) v
                      regA .= r
                      pc += 1
runInstr (SUB reg) = do cpu <- get
                        r <- withFlags (-) All (cpu ^. regA) (cpu ^. fromReg reg)
                        regA .= r
                        pc += 1
runInstr (SBB M) = do cpu <- get
                      rom <- ask
                      let addr = encode (cpu ^. regH) (cpu ^. regL)
                          v = if addr .&. 0x8000 == 0
                            then (cpu ^. memory) V.! addr
                            else rom V.! addr
                      r  <- withFlags (-) All (cpu ^. regA) v
                      r' <- withFlags (-) All r (cpu ^. flags.cy)
                      regA .= r'
                      pc += 1
runInstr (SBB reg) = do cpu <- get
                        r  <- withFlags (-) All (cpu ^. regA) (cpu ^. fromReg reg)
                        r' <- withFlags (-) All r (cpu ^. flags.cy)
                        regA .= r'
                        pc += 1
runInstr (ANA reg) = do cpu <- get
                        rom <- ask
                        let rep = case reg of
                              M -> let addr = encode (cpu ^. regH) (cpu ^. regL)
                                  in if addr .&. 0x8000 == 0
                                        then (cpu ^. memory) V.! addr
                                        else rom V.! addr
                              _ -> cpu ^. fromReg reg
                        r <- withFlags (.&.) All (cpu ^. regA) rep
                        regA .= r
                        pc += 1
runInstr (XRA reg) = do cpu <- get
                        rom <- ask
                        let rep = case reg of
                              M -> let addr = encode (cpu ^. regH) (cpu ^. regL)
                                  in if addr .&. 0x8000 == 0
                                        then (cpu ^. memory) V.! addr
                                        else rom V.! addr
                              _ -> cpu ^. fromReg reg
                        r <- withFlags xor All (cpu ^. regA) rep
                        regA .= r
                        pc += 1
runInstr (ORA reg) = do cpu <- get
                        rom <- ask
                        let rep = case reg of
                              M -> let addr = encode (cpu ^. regH) (cpu ^. regL)
                                  in if addr .&. 0x8000 == 0
                                        then (cpu ^. memory) V.! addr
                                        else rom V.! addr
                              _ -> cpu ^. fromReg reg
                        r <- withFlags (.|.) All (cpu ^. regA) rep
                        regA .= r
                        pc += 1
runInstr (CMP reg) = do cpu <- get
                        rom <- ask
                        let rep = case reg of
                              M -> let addr = encode (cpu ^. regH) (cpu ^. regL)
                                  in if addr .&. 0x8000 == 0
                                        then (cpu ^. memory) V.! addr
                                        else rom V.! addr
                              _ -> cpu ^. fromReg reg
                        _ <- withFlags (-) All (cpu ^. regA) rep
                        pc += 1
runInstr RET = do cpu <- get
                  let ptr = encode (cpu ^. sph) (cpu ^. spl)
                  pc .= encode ((cpu ^. memory) V.! (ptr + 1)) ((cpu ^. memory) V.! ptr)
                  let (hi,lo) = decode . (+2) . fromIntegral $ ptr
                  spl .= lo
                  sph .= hi
runInstr (CALL addr) = do cpu <- get
                          let (hi,lo) = decode $ (cpu ^. pc) + 3
                              sp = encode (cpu ^. sph) (cpu ^. spl)
                          memory %= \x -> V.update x ([(sp - 1, hi), (sp - 2, lo)] ^. vector)
                          let (h,l) = decode . subtract 2 . fromIntegral $ sp
                          spl .= l
                          sph .= h
                          pc .= addr
runInstr RNZ = do cpu <- get
                  if cpu ^. flags.z /= 1
                    then runInstr RET
                    else pc += 1
runInstr (POP PSW) = do cpu <- get
                        let sp = encode (cpu ^. sph) (cpu ^. spl)
                            flag = (cpu ^. memory) V.! sp
                        flags.s  .= (flag `shiftR` 7) .&. 0x01
                        flags.z  .= (flag `shiftR` 6) .&. 0x01
                        flags.ac .= (flag `shiftR` 4) .&. 0x01
                        flags.p  .= (flag `shiftR` 2) .&. 0x01
                        flags.cy .= (flag `shiftR` 0) .&. 0x01
                        regA .= (cpu ^. memory) V.! (sp + 1)
                        let (hi,lo) = decode . (+2) . fromIntegral $ sp
                        sph .= hi
                        spl .= lo
                        pc += 1
runInstr (POP reg) = do cpu <- get
                        pc += 1
                        let sp = encode (cpu ^. sph) (cpu ^. spl)
                        succReg reg .= (cpu ^. memory) V.! sp
                        fromReg reg .= (cpu ^. memory) V.! (sp + 1)
                        let (hi,lo) = decode . (+ 2) . fromIntegral $ sp
                        sph .= hi
                        spl .= lo
runInstr (JMP addr) = pc .= addr
runInstr (JNZ addr) = do cpu <- get
                         if cpu ^. flags.z /= 1
                           then runInstr (JMP addr)
                           else pc += 3
runInstr (CNZ addr) = do cpu <- get
                         if cpu ^. flags.z /= 1
                           then runInstr (CALL addr)
                           else pc += 3
runInstr (PUSH PSW) = do cpu <- get
                         let flag =  fromIntegral (cpu ^. flags.s `shiftL` 7)
                                 .|. fromIntegral (cpu ^. flags.z `shiftL` 6)
                                 .|. fromIntegral (cpu ^. flags.ac `shiftL` 4)
                                 .|. fromIntegral (cpu ^. flags.p `shiftL` 2)
                                 .|. bit 1
                                 .|. fromIntegral (cpu ^. flags.cy `shiftL` 0)
                             a = cpu ^. regA
                             sp = encode (cpu ^. sph) (cpu ^. spl)
                         memory %= \x -> V.update x ([(sp - 1, a), (sp - 2, flag)] ^. vector)
                         let (hi,lo) = decode . subtract 2 . fromIntegral $ sp
                         sph .= hi
                         spl .= lo
                         pc += 1
runInstr (PUSH reg) = do cpu <- get
                         pc += 1
                         let sp = encode (cpu ^. sph) (cpu ^. spl)
                         memory %= \x -> V.update x ([ (sp - 2, cpu ^. succReg reg)
                                                    , (sp - 1, cpu ^. fromReg reg)
                                                    ] ^. vector)
                         let (hi,lo) = decode . subtract 2 . fromIntegral $ sp
                         sph .= hi
                         spl .= lo
runInstr (ADI d8) = do pc += 2
                       cpu <- get
                       r <- withFlags (+) All (cpu ^. regA) d8
                       regA .= r
runInstr (RST 0) = runInstr (CALL 0x00)
runInstr (RST 1) = runInstr (CALL 0x08)
runInstr (RST 2) = runInstr (CALL 0x10)
runInstr (RST 3) = runInstr (CALL 0x18)
runInstr (RST 4) = runInstr (CALL 0x20)
runInstr (RST 5) = runInstr (CALL 0x28)
runInstr (RST 6) = runInstr (CALL 0x30)
runInstr (RST 7) = runInstr (CALL 0x38)
runInstr RZ = do cpu <- get
                 if cpu ^. flags.z == 1
                   then runInstr RET
                   else pc += 1
runInstr (JZ addr) = do cpu <- get
                        if cpu ^. flags.z == 1
                          then runInstr (JMP addr)
                          else pc += 3
runInstr (CZ addr) = do cpu <- get
                        if cpu ^. flags.z == 1
                          then runInstr (CALL addr)
                          else pc += 3
runInstr (ACI d8) = do cpu <- get
                       pc += 2
                       r  <- withFlags (+) All (cpu ^. regA) (cpu ^. flags.cy)
                       r' <- withFlags (+) All r d8
                       regA .= r'
runInstr RNC = do cpu <- get
                  if cpu ^. flags.cy == 0
                    then runInstr RET
                    else pc += 1
runInstr (JNC addr) = do cpu <- get
                         if cpu ^. flags.cy == 0
                           then runInstr (CALL addr)
                           else pc += 3
runInstr (OUT d8) = do cpu <- get
                       ports %= \x -> V.update x (V.fromList [(fromIntegral d8, cpu ^. regA)])
                       pc += 2
runInstr (CNC addr) = do cpu <- get
                         if cpu ^. flags.cy == 0
                           then runInstr (CALL addr)
                           else pc += 3
runInstr (SUI d8) = do cpu <- get
                       withFlags (-) All (cpu ^. regA) d8 >>= (regA .=)
                       pc += 2
runInstr RC = do cpu <- get
                 if cpu ^. flags.cy == 1
                   then runInstr RET
                   else pc += 1
runInstr (JC addr) = do cpu <- get
                        if cpu ^. flags.cy == 1
                          then runInstr (JMP addr)
                          else pc += 3
runInstr (IN d8) = do cpu <- get
                      regA .= (cpu ^. ports) V.! fromIntegral d8
                      pc += 2
runInstr (CC addr) = do cpu <- get
                        if cpu ^. flags.cy == 1
                          then runInstr (CALL addr)
                          else pc += 3
runInstr (SBI d8) = do cpu <- get
                       pc += 2
                       r  <- withFlags (-) All (cpu ^. regA) (cpu ^. flags.cy)
                       r' <- withFlags (-) All r d8
                       regA .= r'
runInstr RPO = do cpu <- get
                  if cpu ^. flags.p == 0 -- is odd?
                    then runInstr RET
                    else pc += 1
runInstr (JPO addr) = do cpu <- get
                         if cpu ^. flags.p == 0
                           then runInstr (JMP addr)
                           else pc += 3
runInstr XTHL = do cpu <- get
                   let sp = encode (cpu ^. sph) (cpu ^. spl)
                       h = cpu ^. regH
                       l = cpu ^. regL
                   regH .= (cpu ^. memory) V.! (sp + 1)
                   regL .= (cpu ^. memory) V.! sp
                   memory %= \x -> V.update x ([(sp, l), (sp + 1, h)] ^. vector)
                   pc += 1
runInstr (CPO addr) = do cpu <- get
                         if cpu ^. flags.p == 0
                           then runInstr (CALL addr)
                           else pc += 3
runInstr (ANI d8) = do cpu <- get
                       withFlags (.&.) All (cpu ^. regA) d8 >>= (regA .=)
                       pc += 2
runInstr RPE = do cpu <- get
                  if cpu ^. flags.p == 1 -- is even?
                    then runInstr RET
                    else pc += 1
runInstr PCHL = do cpu <- get
                   pc .= encode (cpu ^. regH) (cpu ^. regL)
runInstr (JPE addr) = do cpu <- get
                         if cpu ^. flags.p == 1
                           then runInstr (JMP addr)
                           else pc += 3
runInstr XCHG = do cpu <- get
                   let h = cpu ^. regH
                       l = cpu ^. regL
                   regH .= cpu ^. regD
                   regL .= cpu ^. regE
                   regD .= h
                   regE .= l
                   pc += 1
runInstr (CPE addr) = do cpu <- get
                         if cpu ^. flags.p == 1
                           then runInstr (CALL addr)
                           else pc += 3
runInstr (XRI d8) = do cpu <- get
                       withFlags xor All (cpu ^. regA) d8 >>= (regA .=)
                       pc += 2
runInstr RP = do cpu <- get
                 if cpu ^. flags.s == 0 -- positive
                   then runInstr RET
                   else pc += 1
runInstr (JP addr) = do cpu <- get
                        if cpu ^. flags.s == 0 -- positive
                          then runInstr (JMP addr)
                          else pc += 3
runInstr DI = do interrupt .= False
                 pc += 1
runInstr (CP addr) = do cpu <- get
                        if cpu ^. flags.s == 0 -- positive
                          then runInstr (CALL addr)
                          else pc += 3
runInstr (ORI d8) = do cpu <- get
                       withFlags (.|.) All (cpu ^. regA) d8 >>= (regA .=)
                       pc += 2
runInstr RM = do cpu <- get
                 if cpu ^. flags.s == 1 -- negative
                   then runInstr RET
                   else pc += 1
runInstr SPHL = do cpu <- get
                   sph .= cpu ^. regH
                   spl .= cpu ^. regL
                   pc += 1
runInstr (JM addr) = do cpu <- get
                        if cpu ^. flags.s == 1 --negative
                          then runInstr (JMP addr)
                          else pc += 3
runInstr EI = do interrupt .= True
                 pc += 1
runInstr (CM addr) = do cpu <- get
                        if cpu ^. flags.s == 1 -- negative
                          then runInstr (CALL addr)
                          else pc += 3
runInstr (CPI d8) = do cpu <- get
                       _ <- withFlags (-) All (cpu ^. regA) d8
                       pc += 2
runInstr _ = error "unimplemented!"

loadNext :: CPU (Maybe Instruction)
loadNext = do
  rom <- lift ask
  cpu <- get
  return $ rom V.!? fromIntegral (cpu ^. pc)

runCPU :: CPU () -> Raw -> ROM -> IO (CPUState Word8 Word16)
runCPU cpu bin rom = execStateT (runReaderT (runReaderT cpu bin) rom) newCPU

debug :: CPU ()
debug = do
  i <- loadNext
  cpu <- get
  case i of
    Nothing -> return ()
    Just ins -> do
      runInstr ins
      let sp = (encode (cpu ^. sph) (cpu ^. spl) ) :: Int
      liftIO $ print $ show i ++ "   " ++ showHex (cpu ^. pc) "" ++ " | "
                              ++ "sp: " ++ showHex sp "" ++ " | "
                              ++ "addr: " ++ showHex ((cpu ^. memory) V.! (sp + 1)) "" ++ " "
                                          ++ showHex ((cpu ^. memory) V.! sp) ""
                              ++ " | z:" ++ show (cpu ^. flags.z)
                              ++ " | A:" ++ show (cpu ^. regA)
                              ++ " | B:" ++ show (cpu ^. regB)
                              ++ " | C:" ++ show (cpu ^. regC)
                              ++ " | D:" ++ show (cpu ^. regD)
                              ++ " | H:" ++ show (cpu ^. regH)
                              ++ " | L:" ++ show (cpu ^. regL)
                              -- ++ " | " ++ show ((cpu ^. memory) V.! 1700) ++ " " ++ show ((cpu ^. memory) V.! 1701) ++ " " ++ show ((cpu ^. memory) V.! 1702)
      debug

withCycle :: Int -> CPU ()
withCycle c = do
  i <- loadNext
  when (c /= 0) $
    case i of
      Nothing  -> return ()
      Just ins -> runInstr ins >> withCycle (c - 1)

runTest = let str = "/Users/clinix/Downloads/invaders/invaders.bin"
          in do
               join $ pure (runCPU (withCycle 40000 >> debug)) <*> raw str <*> parse str
