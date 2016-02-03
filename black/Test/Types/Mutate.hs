module Test.Types.Mutate (module Test.Types) where

import FitSpec.Mutable
import FitSpec.ShowMutable
import Test.Types

-- {- Standard implementation:
instance Mutable Nat   where lsMutants = lsMutantsEq
instance Mutable Int2  where lsMutants = lsMutantsEq
instance Mutable Int3  where lsMutants = lsMutantsEq
instance Mutable Int4  where lsMutants = lsMutantsEq
instance Mutable UInt2 where lsMutants = lsMutantsEq
instance Mutable UInt3 where lsMutants = lsMutantsEq
instance Mutable UInt4 where lsMutants = lsMutantsEq
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
instance ShowMutable Nat   where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable Int2  where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable Int3  where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable Int4  where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable UInt2 where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable UInt3 where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable UInt4 where mutantS = mutantSEq; showMutant _ = show
