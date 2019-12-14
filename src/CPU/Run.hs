{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module CPU.Run
  ( load
  , step
  ) where

import CPU
import CPU.Instructions.Decodes
import CPU.Instructions.Execute
import CPU.Instructions.Jumps
import CPU.Instructions.Length
import CPU.Instructions.Opcode
import CPU.Instructions.Timing
import CPU.Program

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable         ((//))
import Prelude                      hiding (break)

import qualified Data.Vector.Storable as DVS

load :: Int -> Program -> CPU -> CPU
load o (Program bin) cpu = do
  let w = zip [o..] (DVS.toList bin)
  cpu & field @"mem" %~ (// w)

step :: CPU -> CPU
step cpu =
  cpu & execute ins & updatePC & setTim
  where ins      = decode @Opcode mem'
        mem'     = DVS.drop (fromIntegral (pc cpu)) (mem cpu)
        len      = fromIntegral $ insLength ins
        updatePC = if jumps ins then id else field @"pc" %~ (+ len)
        setTim   = field @"tim" .~ t
        t        = cycles ins
