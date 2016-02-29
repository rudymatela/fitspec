-- | FitSpec Test Types:
-- Nat,
-- Int2, Int3, Int4,
-- UInt2, UInt3, UInt4.
--
-- This module basically re-exports llcheck's Test.Types module
-- and defines Mutable and ShowMutable instances for the types
-- defined there.
module FitSpec.TestTypes (module Test.Types) where

import FitSpec.Mutable
import FitSpec.ShowMutable
import Test.Types

-- {- Standard implementation:
instance Mutable Nat   where tMutants = tMutantsEq
instance Mutable Int1  where tMutants = tMutantsEq
instance Mutable Int2  where tMutants = tMutantsEq
instance Mutable Int3  where tMutants = tMutantsEq
instance Mutable Int4  where tMutants = tMutantsEq
instance Mutable UInt1 where tMutants = tMutantsEq
instance Mutable UInt2 where tMutants = tMutantsEq
instance Mutable UInt3 where tMutants = tMutantsEq
instance Mutable UInt4 where tMutants = tMutantsEq
-- -}
{- Alternative implementation:
instance Mutable Nat   where mutants = mutantsIntegral
instance Mutable Int2  where mutants = mutantsIntegral
instance Mutable Int3  where mutants = mutantsIntegral
instance Mutable Int4  where mutants = mutantsIntegral
instance Mutable UInt2 where mutants = mutantsIntegral
instance Mutable UInt3 where mutants = mutantsIntegral
instance Mutable UInt4 where mutants = mutantsIntegral
-- -}
instance ShowMutable Nat   where mutantS = mutantSEq
instance ShowMutable Int1  where mutantS = mutantSEq
instance ShowMutable Int2  where mutantS = mutantSEq
instance ShowMutable Int3  where mutantS = mutantSEq
instance ShowMutable Int4  where mutantS = mutantSEq
instance ShowMutable UInt1 where mutantS = mutantSEq
instance ShowMutable UInt2 where mutantS = mutantSEq
instance ShowMutable UInt3 where mutantS = mutantSEq
instance ShowMutable UInt4 where mutantS = mutantSEq
