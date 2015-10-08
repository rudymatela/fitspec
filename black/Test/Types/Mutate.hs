module Test.Types.Mutate () where

import Mutate
import Test.Types

instance Mutable Nat   where szMutants = lsMutantsEq
instance Mutable UInt2 where szMutants = lsMutantsEq
instance Mutable UInt3 where szMutants = lsMutantsEq
instance Mutable UInt4 where szMutants = lsMutantsEq
instance ShowMutable Nat   where showMutant = showMutantEq
instance ShowMutable UInt2 where showMutant = showMutantEq
instance ShowMutable UInt3 where showMutant = showMutantEq
instance ShowMutable UInt4 where showMutant = showMutantEq
