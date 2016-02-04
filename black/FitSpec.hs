-- | FitSpec
module FitSpec
  ( module FitSpec.Engine
  , module FitSpec.Main

  , module FitSpec.Mutable
  , module FitSpec.ShowMutable
  , module FitSpec.Derive
  , module FitSpec.TestTypes

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
import FitSpec.TestTypes

import Test.Check
