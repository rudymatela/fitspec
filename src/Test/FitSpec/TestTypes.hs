-- |
-- Module      : Test.FitSpec.TestTypes
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- FitSpec's Test Types:
-- 'Nat',
-- 'Int2', 'Int3', 'Int4',
-- 'UInt2', 'UInt3', 'UInt4'.
--
-- This module re-exports "Test.LeanCheck.Utils.Types" module
-- and defines 'Mutable' and 'ShowMutable' instances for the types
-- defined there.
module Test.FitSpec.TestTypes (module Test.LeanCheck.Utils.Types) where

import Test.FitSpec.Mutable
import Test.FitSpec.ShowMutable
import Test.LeanCheck.Utils.Types

-- {- Standard implementation:
instance Mutable Nat   where mutiers = mutiersEq
instance Mutable Int1  where mutiers = mutiersEq
instance Mutable Int2  where mutiers = mutiersEq
instance Mutable Int3  where mutiers = mutiersEq
instance Mutable Int4  where mutiers = mutiersEq
instance Mutable Word1 where mutiers = mutiersEq
instance Mutable Word2 where mutiers = mutiersEq
instance Mutable Word3 where mutiers = mutiersEq
instance Mutable Word4 where mutiers = mutiersEq
-- -}
{- Alternative implementation:
instance Mutable Nat   where mutants = mutantsIntegral
instance Mutable Int2  where mutants = mutantsIntegral
instance Mutable Int3  where mutants = mutantsIntegral
instance Mutable Int4  where mutants = mutantsIntegral
instance Mutable Word2 where mutants = mutantsIntegral
instance Mutable Word3 where mutants = mutantsIntegral
instance Mutable Word4 where mutants = mutantsIntegral
-- -}
instance ShowMutable Nat   where mutantS = mutantSEq
instance ShowMutable Int1  where mutantS = mutantSEq
instance ShowMutable Int2  where mutantS = mutantSEq
instance ShowMutable Int3  where mutantS = mutantSEq
instance ShowMutable Int4  where mutantS = mutantSEq
instance ShowMutable Word1 where mutantS = mutantSEq
instance ShowMutable Word2 where mutantS = mutantSEq
instance ShowMutable Word3 where mutantS = mutantSEq
instance ShowMutable Word4 where mutantS = mutantSEq
