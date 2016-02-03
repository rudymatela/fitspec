-- | FitSpec
module FitSpec
  ( module FitSpec.Engine
  , module FitSpec.Main

  , module Mutable
  , module ShowMutable
  , module Derive

  , module Test.Types.Mutate
  , module Test.Check
  )
where
-- TODO: Explictly export *only* needed functions
-- TODO: Write short introduction to FitSpec

import FitSpec.Engine
import FitSpec.Main

import FitSpec.Mutable
import FitSpec.ShowMutable hiding (showMutantN)
import FitSpec.Derive hiding (showMutantN)

import Test.Types.Mutate
import Test.Check
