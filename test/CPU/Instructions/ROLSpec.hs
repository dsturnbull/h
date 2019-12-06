{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Instructions.ROLSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.DE
import CPU.Instructions.IN
import CPU.Instructions.LDA
import CPU.Instructions.LDX
import CPU.Instructions.ROL
import CPU.Instructions.SE
import CPU.Instructions.STA

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Vector.Storable  hiding (reverse, sum, zipWith)
import Data.Word
import Foreign.Marshal.Utils (fromBool)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "rol" $ do
  it "acc" $ requireProperty $ do
    cpu     <- forAll $ genCPU 1
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll $ bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldaImm w
             & rolAcc
    let r = w `shiftL` 1 .|. (c & fromBool)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    addr    <- forAll $ word8 (linear minBound maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll $ bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldaImm w
             & staZpg addr
             & rolZpg addr
             & ldaZpg addr
    let r = w `shiftL` 1 .|. (c & fromBool)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    x       <- forAll $ word8 (linear 10 20)
    addr    <- forAll $ word8 (linear minBound (maxBound - 20))
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll $ bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldxImm x
             & ldaImm w
             & staZpgX addr
             & rolZpgX addr
             & ldaZpgX addr
    let r = w `shiftL` 1 .|. (c & fromBool)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    addr    <- forAll $ word16 (linear minBound (memSize - 1))
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll $ bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldaImm w
             & staAbs addr
             & rolAbs addr
             & ldaAbs addr
    let r = w `shiftL` 1 .|. (c & fromBool)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    x       <- forAll $ word8 (linear 10 20)
    addr    <- forAll $ word16 (linear minBound (memSize - 20))
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll $ bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldxImm x
             & ldaImm w
             & staAbsX addr
             & rolAbsX addr
             & ldaAbsX addr
    let r = w `shiftL` 1 .|. (c & fromBool)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "double multi-byte numbers" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    a       <- forAll $ word8 (linear minBound maxBound)
    b       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll $ word8 (linear minBound maxBound)
    d       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (memSize - 20))
    let cpu' = cpu
             & ldaImm a & staAbsX addr & inx
             & ldaImm b & staAbsX addr & inx
             & ldaImm c & staAbsX addr & inx
             & ldaImm d & staAbsX addr & inx
             & dex & rolAbsX addr
             & dex & rolAbsX addr
             & dex & rolAbsX addr
             & dex & rolAbsX addr
    annotateShow cpu'
    let a' = (cpu' & mem) ! (fromIntegral addr)
    let b' = (cpu' & mem) ! (fromIntegral addr + 1)
    let c' = (cpu' & mem) ! (fromIntegral addr + 2)
    let d' = (cpu' & mem) ! (fromIntegral addr + 3)
    let t  :: Word32 = sum $ zipWith (shiftL . fromIntegral) (reverse [a, b, c, d])     [0, 8, 16, 24]
    let t' :: Word32 = sum $ zipWith (shiftL . fromIntegral) (reverse [a', b', c', d']) [0, 8, 16, 24]
    annotateShow [a, b, c, d]
    annotateShow t
    annotateShow [a', b', c', d']
    annotateShow t'
    t' === t * 2
    (cpu' & p & carry) === (t .&. 0x80000000 /= 0)
