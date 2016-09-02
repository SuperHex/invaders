{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase #-}

-- | Monaddic interface for CPU emulation

module MonadCPU
  ( Address
  , MonadCPU (..)
  ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.ByteString as B
import           Data.Word                   (Word16, Word8)
import           Disassembler
import Data.Bits
import           Utils                       (decode, encode)

type Address = Word16

class (Monad m) => MonadCPU (m :: * -> *) where
  load :: m Word8
  loadAddr :: m Word16
  loadAddr = do
    lsb <- load
    msb <- load
    return $ encode msb lsb
  peek :: m CPUState
  jump :: Address -> m ()
  readMem :: Address -> m Word8
  writeMem :: Address -> Word8 -> m ()
  modifyMem :: Address -> (Word8 -> Word8) -> m ()
  modifyMem addr f = do
    w8 <- readMem addr
    writeMem addr (f w8)
  {-# MINIMAL load, peek, jump, readMem, writeMem #-}

data Flags = Flags
  { _z   ::  {-# UNPACK #-} !Word8
  , _s   ::  {-# UNPACK #-} !Word8
  , _p   ::  {-# UNPACK #-} !Word8
  , _cy  ::  {-# UNPACK #-} !Word8
  , _ac  ::  {-# UNPACK #-} !Word8
  , _pad ::  {-# UNPACK #-} !Word8
  }

data CPUState = CPU
  { _regA      ::  {-# UNPACK #-} !Word8
  , _regBC     ::  {-# UNPACK #-} !(Word8, Word8)
  , _regDE     ::  {-# UNPACK #-} !(Word8, Word8)
  , _regHL     ::  {-# UNPACK #-} !(Word8, Word8)
  , _sp        ::  {-# UNPACK #-} !(Word8, Word8)
  , _pc        ::  {-# UNPACK #-} !Word16
  , _memory    ::  VUM.IOVector Word8
  , _flags ::  Flags
  , _interrupt ::  {-# UNPACK #-} !Bool
  , _ports     ::  VUM.IOVector Word8
  }

makeLenses ''Flags
makeLenses ''CPUState

instance MonadCPU (StateT CPUState IO) where
  load = do
    cpu <- get
    inst <- readMem (cpu ^. pc)
    pc += 1
    return inst
  jump addr = pc .= addr
  peek  = get
  readMem addr = do
    cpu <- get
    VUM.read (cpu ^. memory) (fromIntegral addr)
  writeMem addr word = do
    cpu <- get
    VUM.write (cpu ^. memory) (fromIntegral addr) word

fromReg :: Reg -> Lens CPUState CPUState Word8 Word8
fromReg A = regA
fromReg B = regBC . _1
fromReg C = regBC . _2
fromReg D = regDE . _1
fromReg E = regDE . _2
fromReg H = regHL . _1
fromReg L = regHL . _2

fromRegPair :: Reg -> Lens CPUState CPUState (Word8,Word8) (Word8,Word8)
fromRegPair = \case B -> regBC
                    D -> regDE
                    H -> regHL
                    SP -> sp

checkZero, checkSign, checkPop, checkAC ::
  (MonadCPU m, MonadState CPUState m) =>
  (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> m Word8

checkCy :: (MonadCPU m, MonadState CPUState m) =>
  (Word16 -> Word16 -> Word16) -> Word8 -> Word8 -> m Word8

checkZero f a b = do
  if f a b == 0x00
    then flags.z .= 1
    else flags.z .= 0
  return $ f a b
checkSign f a b = do
  if f a b .&. 0x80 == 0x80
    then flags.s .= 1
    else flags.s .= 0
  return $ f a b
checkPop f a b = do
  if popCount (f a b) `mod` 2 == 0
    then flags.p .= 1
    else flags.p .= 0
  return $ f a b
checkAC f a b = do
  if a .&. 0x0F `f` b .&. 0x0F > 0x0F
    then flags.ac .= 1
    else flags.ac .= 0
  return $ f a b
checkCy f a b = do
  if f a' b' > 0xFF
    then flags.cy .= 1
    else flags.cy .= 0
  return $ fromIntegral (f a' b')
  where a' = fromIntegral a :: Word16
        b' = fromIntegral b :: Word16

checkZSPAC f a b = do
  checkZero f a b
  checkSign f a b
  checkPop  f a b
  checkAC   f a b

checkFlags :: (MonadCPU m, MonadState CPUState m)
           => (forall a. (Num a, Bits a) => a -> a -> a) -> Word8 -> Word8 -> m Word8
checkFlags f a b = checkZSPAC f a b >> checkCy f a b

encode' = uncurry encode

-- eval :: (MonadCPU m, MonadState CPUState m) => Instruction -> m ()
eval :: Instruction ->  StateT CPUState IO ()
eval ins = do
  cpu <- peek
  case ins of
    NOP -> return ()
    LXI reg -> do
      addr <- loadAddr
      fromRegPair reg .= decode addr
    STAX reg ->
      writeMem (encode' $ cpu ^. fromRegPair reg) (cpu ^. regA)
    INX reg ->
      modifyMem (encode' $ cpu ^. fromRegPair reg) (+1)
    INR M -> do
      x <- readMem (encode' $ cpu ^. regHL)
      writeMem (encode' (cpu ^. regHL)) =<< checkZSPAC (+) x 1
    INR reg -> (fromReg reg .=) =<< checkZSPAC (+) (cpu ^. fromReg reg) 1
    DCR M -> do
      x <- readMem (encode' (cpu ^. regHL))
      writeMem (encode' (cpu ^. regHL)) =<< checkZSPAC (-) x 1
    DCR reg ->
      (fromReg reg .=) =<< checkZSPAC (-) (cpu ^. fromReg reg) 1
    MVI M -> do
      byte <- load
      writeMem (encode' (cpu ^. regHL)) byte
    MVI reg ->
      (fromReg reg .=) =<< load
    RLC -> do
      let b = (cpu ^. regA .&. 0x80) `shiftR` 7
      regA %= \x -> (x `shiftL` 1) .|. b
      flags.cy .= b
    DAD reg -> do
      let x = encode' (cpu ^. regHL)
          y = encode' (cpu ^. fromRegPair reg)
          ans = x + y
      when (ans > 0xFFFF) (flags.cy .= 1)
      regHL .= decode ans
    LDAX reg ->
      (regA .=) =<< readMem (encode' (cpu ^. fromRegPair reg))
    DCX reg ->
      fromRegPair reg .= (decode . subtract 1 . encode') (cpu ^. fromRegPair reg)
    RRC -> do
      let b = (cpu ^. regA) .&. 0x01
      regA %= \x -> (x `shiftR` 1) .|. (b `shiftL` 7)
      flags.cy .= b
    RAL -> do
      let c = cpu ^. flags.cy
          b = (cpu ^. regA .&. 0x80) `shiftR` 7
      regA %= \x -> (x `shiftL` 1) .|. c
      flags.cy .= b
    RAR -> do
      let c = cpu ^. flags.cy
          b = cpu ^. regA .&. 0x01
      regA %= \x -> (x `shiftR` 1) .|. (c `shiftL` 7)
      flags.cy .= b
    RIM ->
      regA .=  (0 `shiftL` 7)              -- Serial input data bit, not used for now.
           .|. (0 `shiftL` 6)              -- Pending interrupt of RST 7,6,5. not used fot now.
           .|. (0 `shiftL` 5)
           .|. (0 `shiftL` 4)
           .|. (if cpu ^. interrupt
                  then 0x01
                  else 0x00)               -- IE
           .|. (1 `shiftL` 2)              -- Interrupt mask of RST 7,6,5.
           .|. (1 `shiftL` 1)
           .|. (1 `shiftL` 0)
    SHLD -> do
      addr <- loadAddr
      let (h,l) = cpu ^. regHL
      writeMem addr l
      writeMem (addr + 1) h
    DAA -> do
      let decode4 word = (,) ((word .&.0xF0) `shiftR` 4) (word .&. 0x0F)
          (_,lsb) = decode4 (cpu ^. regA)
      tmp <- if lsb > 9 || (cpu ^. flags.ac) == 1
                then checkFlags (+) (cpu ^. regA) 6
                else return (cpu ^. regA)
      let (msb,_) = decode4 tmp
      res <- if msb > 9 || (cpu ^. flags.cy) == 1
                then checkFlags (+) tmp (6 `shiftL` 4)
                else return tmp
      regA .= res
    LHLD -> do
      addr <- loadAddr
      (regHL .=) =<< liftM2 (,)
                     (readMem (addr + 1))
                     (readMem addr)
    CMA ->
      regA %= xor 0xff
    SIM -> undefined                                 -- TODO
    STA -> do
      addr <- loadAddr
      writeMem addr (cpu ^. regA)
    STC ->
      flags.cy .= 1
    LDA -> do
      addr <- loadAddr
      (regA .=) =<< readMem addr
    CMC ->
      flags.cy %= xor (bit 0)
    MOV reg M ->
      (fromReg reg .=) =<< readMem (encode' (cpu ^. regHL))
    MOV M reg ->
      writeMem (encode' (cpu ^. regHL)) (cpu ^. fromReg reg)
    MOV r1 r2 ->
      fromReg r1 .= cpu ^. fromReg r2
    HLT -> undefined
    ADD M -> do
      x <- readMem (encode' (cpu ^. regHL))
      (regA .=) =<< checkFlags (+) (cpu ^. regA) x
    ADD reg ->
      (regA .=) =<< checkFlags (+) (cpu ^. regA) (cpu ^. fromReg reg)
    ADC M -> do
      x <- readMem (encode' (cpu ^. regHL))
      (regA .=) =<< (checkFlags (+) x >=>
                     checkFlags (+) (cpu ^. flags.cy)) (cpu ^. regA)
    ADC reg ->
      (regA .=) =<< (checkFlags (+) (cpu ^. fromReg reg) >=>
                     checkFlags (+) (cpu ^. flags.cy)) (cpu ^. regA)
    SUB M ->
      (regA .=) =<< checkFlags (-) (cpu ^. regA) =<< readMem (encode' (cpu ^. regHL))
    SUB reg ->
      (regA .=) =<< checkFlags (-) (cpu ^. regA) (cpu ^. fromReg reg)
    SBB reg -> do
      x <- case reg of
        M -> readMem (encode' (cpu ^. regHL))
        _ -> return (cpu ^. fromReg reg)
      (regA .=) =<< (checkFlags (-) (cpu ^. regA) >=>
                     \sub -> checkFlags (-) sub (cpu ^. flags.cy)) x
    ANA reg -> do
      x <- case reg of
        M -> readMem (encode' (cpu ^. regHL))
        _ -> return $ cpu ^. fromReg reg
      (regA .=) =<< checkFlags (.&.) (cpu ^. regA) x
    XRA reg -> do
      x <- case reg of
        M -> readMem (encode' (cpu ^. regHL))
        _ -> return $ cpu ^. fromReg reg
      (regA .=) =<< checkFlags xor (cpu ^. regA) x
    ORA reg -> do
      x <- case reg of
        M -> readMem (encode' (cpu ^. regHL))
        _ -> return $ cpu ^. fromReg reg
      (regA .=) =<< checkFlags (.|.) (cpu ^. regA) x
    CMP reg -> do
      x <- case reg of
        M -> readMem (encode' (cpu ^. regHL))
        _ -> return $ cpu ^. fromReg reg
      void $ checkFlags (-) (cpu ^. regA) x
    RET -> do
      (pc .=) =<< liftM2 encode
                         (readMem (encode' (cpu ^. sp) + 1))
                         (readMem (encode' (cpu ^. sp)))
      sp %= decode . (+2) . encode'
    CALL -> do
      addr <- loadAddr
      let (msb,lsb) = decode (cpu ^. pc)
          stack = encode' (cpu ^. sp)
      writeMem (stack - 1) msb
      writeMem (stack - 2) lsb
      pc .= addr
      sp %= decode . subtract 2 . encode'
    POP PSW -> do
      stored <- readMem (encode' (cpu ^. sp))
      flags.s  .= (stored `shiftR` 7) .&. 1
      flags.z  .= (stored `shiftR` 6) .&. 1
      flags.ac .= (stored `shiftR` 4) .&. 1
      flags.p  .= (stored `shiftR` 2) .&. 1
      flags.cy .= (stored `shiftR` 0) .&. 1
      (regA .=) =<< readMem (encode' (cpu ^. sp) + 1)
      sp %= decode . (+2) . encode'
    POP reg -> do
      let stack = encode' (cpu ^. sp)
      lsb <- readMem stack
      msb <- readMem (stack + 1)
      fromRegPair reg .= (msb,lsb)
      sp %= decode . (+2) . encode'
    RNZ ->
      when (cpu ^. flags.z == 0) (eval RET)
    JMP ->
      jump =<< loadAddr
    JNZ ->
      if cpu ^. flags.z == 0
         then eval JMP
         else void loadAddr
    CNZ ->
      if cpu ^. flags.z == 0
         then eval CALL
         else void loadAddr
    PUSH PSW -> do
      let stack = encode' (cpu ^. sp)
      writeMem (stack - 1) (cpu ^. regA)
      writeMem (stack - 2) $  fromIntegral (cpu ^. flags.s  `shiftL` 7)
                          .|. fromIntegral (cpu ^. flags.z  `shiftL` 6)
                          .|. fromIntegral (cpu ^. flags.ac `shiftL` 4)
                          .|. fromIntegral (cpu ^. flags.p  `shiftL` 2)
                          .|. bit 1
                          .|. fromIntegral (cpu ^. flags.cy `shiftL` 0)
      sp %= decode . subtract 2 . encode'
    PUSH reg -> do
      let stack = encode' (cpu ^. sp)
          (msb,lsb) = cpu ^. fromRegPair reg
      writeMem (stack - 1) msb
      writeMem (stack - 2) lsb
      sp %= decode . subtract 2 . encode'
    ADI -> do
      num <- load
      (regA .=) =<< checkFlags (+) (cpu ^. regA) num
    RST n -> do
      let (msb,lsb) = decode (cpu ^. pc)
          stack = encode' (cpu ^. sp)
      writeMem (stack - 1) msb
      writeMem (stack - 2) lsb
      sp %= decode . subtract 2 . encode'
      jump (fromIntegral n * 8)
    RZ ->
      when (cpu ^. flags.z == 1) (eval RET)
    JZ ->
      if cpu ^. flags.z == 1
         then eval JMP
         else void loadAddr
    CZ ->
      if cpu ^. flags.z == 1
         then eval CALL
         else void loadAddr
    ACI -> do
      num <- load
      (regA .=) =<< (checkFlags (+) (cpu ^. regA) >=>
                     checkFlags (+) (cpu ^. flags.cy)) num
    RNC ->
      when (cpu ^. flags.cy == 0) (eval RET)
    JNC ->
      if cpu ^. flags.cy == 0
         then eval JMP
         else void loadAddr
    IN -> do
      idx <- fromIntegral <$> load
      (regA .=) =<< VUM.read (cpu ^. ports) idx
    OUT -> do
      idx <- fromIntegral <$> load
      VUM.write (cpu ^. ports) idx (cpu ^. regA)
    CNC ->
      if cpu ^. flags.cy == 0
         then eval CALL
         else void loadAddr
    SUI -> do
      num <- load
      (regA .=) =<< checkFlags (+) (cpu ^. regA) num
    RC ->
      when (cpu ^. flags.cy == 1) (eval RET)
    JC ->
      if cpu ^. flags.cy == 1
         then eval JMP
         else void loadAddr
    CC ->
      if cpu ^. flags.cy == 1
         then eval CALL
         else void loadAddr
    SBI -> do
      num <- load
      (regA .=) =<< (checkFlags (-) (cpu ^. regA) >=>
                    \x -> checkFlags (-) x (cpu ^. flags.cy)) num
    RPO ->
      when (cpu ^. flags.p == 0) (eval RET)         -- is odd ?
    JPO ->
      if cpu ^. flags.p == 0
        then eval JMP
        else void loadAddr
    XTHL -> do
      let (h,l) = cpu ^. regHL
          stack = encode' (cpu ^. sp)
      (regHL .=) =<< liftM2 (,)
                            (readMem (stack + 1))
                            (readMem stack)
      writeMem stack l
      writeMem (stack + 1) h
    CPO ->
      if cpu ^. flags.p == 0
         then eval CALL
         else void loadAddr
    ANI ->
      (regA .=) =<< checkFlags (.&.) (cpu ^. regA) =<< load
    RPE ->
      when (cpu ^. flags.p == 1) (eval RET)
    PCHL ->
      jump (encode' (cpu ^. regHL))
    JPE ->
      if cpu ^. flags.p == 1
         then eval JMP
         else void loadAddr
    XCHG -> do
      let hl = cpu ^. regHL
      regHL .= cpu ^. regDE
      regDE .= hl
    CPE ->
      if cpu ^. flags.p == 1
         then eval CALL
         else void loadAddr
    XRI ->
      (regA .=) =<< checkFlags xor (cpu ^. regA) =<< load
    RP ->
      when (cpu ^. flags.s == 0) (eval RET)
    JP ->
      if cpu ^. flags.s == 0
         then eval JMP
         else void loadAddr
    DI ->
      interrupt .= False
    CP ->
      if cpu ^. flags.s == 0
         then eval CALL
         else void loadAddr
    ORI ->
      (regA .=) =<< checkFlags (.|.) (cpu ^. regA) =<< load
    RM ->
      when (cpu ^. flags.s == 1) (eval RET)
    SPHL ->
      sp .= cpu ^. regHL
    JM ->
      if cpu ^. flags.s == 1
         then eval JMP
         else void loadAddr
    EI ->
      interrupt .= True
    CM ->
      if cpu ^. flags.s == 1
         then eval CALL
         else void loadAddr
    CPI ->
      void $ checkFlags (-) (cpu ^. regA) =<< load

newCPU :: IO CPUState
newCPU = do
  port <- VUM.replicate 256 0
  return $ CPU 0
               (0,0)
               (0,0)
               (0,0)
               (0,0)
               0
               undefined
               (Flags 0 0 0 0 0 0)
               True
               port

copyROM :: (MonadIO m) => V.Vector Word8 -> VUM.IOVector Word8 -> m ()
copyROM rom v = liftIO $ forM_ [1 .. V.length rom - 1] $
  \idx -> VUM.write v idx (rom V.! idx)

runCPU :: StateT CPUState IO ()
runCPU = do
  ins <- load
  liftIO $ print (disassembly ins)
  eval (disassembly ins)
  runCPU

initCPU :: IO CPUState
initCPU = do
  let file = "/Users/clinix/Downloads/invaders/invaders.bin"
  bin <- B.readFile file
  mem <- VUM.replicate 0x4000 0
  ((V.++ V.replicate 0x2000 0) . V.fromList . B.unpack $ bin) `copyROM` mem
  cpu <- newCPU
  return $ cpu { _memory = mem }

run :: IO ()
run = void $ execStateT runCPU =<< initCPU
