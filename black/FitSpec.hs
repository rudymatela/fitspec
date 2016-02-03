-- | FitSpec
module FitSpec
  ( module FitSpec.Engine
  , module FitSpec.Main

  , module Mutate
  , module Mutate.Show
  , module Mutate.Derive

  , module Test.Types.Mutate
  , module Test.Check
  )
where
-- TODO: Explictly export *only* needed functions
-- TODO: Write short introduction to FitSpec

import FitSpec.Engine
import FitSpec.Main

import Mutate
import Mutate.Show hiding (showMutantN)
import Mutate.Derive hiding (showMutantN)

import Test.Types.Mutate
import Test.Check
