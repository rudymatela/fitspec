-- | Exports "main" functions for FitSpec.
-- They work exactly by 'report' and 'reportWith' but can be customized by
-- command line arguments.
--
-- > main = mainWith args { ... } functions properties
{-# Language DeriveDataTypeable, StandaloneDeriving #-}
module FitSpec.Main
  ( mainWith
  , defaultMain
  , getArgs
  , getArgsWith
  , module FitSpec.Report -- deprecated export, remove later
  )
where

import FitSpec.Report
import System.Console.CmdArgs hiding (args)
import Control.Monad (liftM)
import FitSpec.Mutable
import FitSpec.ShowMutable

deriving instance Data ShowMutantAs
deriving instance Data Args
deriving instance Typeable ShowMutantAs
deriving instance Typeable Args

annotate :: Args -> Args
annotate as = Args
  { nMutants     = nMutants     as  &= name "m"
      &= help "(starting) number of function mutations"
  , nTests       = nTests       as  &= name "n"
      &= help "(starting) number of test values (each prop.)"
  , timeout      = timeout      as  &= name "t" &= name "s"
      &= help "timeout in seconds, 0 for just T*M"
  , names        = names        as
      &= ignore
  , rows         = rows         as
      &= help "how many rows of results to show"
  , verbose      = verbose      as
      &= help "activate verbose output"
  , showMutantAs = showMutantAs as  &= name "a"
      &= help "how to show mutants (tuple / nestedtuple / definition / bindings)"
      &= typ  "type"
  , extra        = extra        as  &= opt ""
      &= help "user-defined meaning: test-type / property-set ?"
  } &= summary "FitSpec"
    &= program "program"
    &= help "Refine property-sets for functional testing"

getArgsWith :: Args -> IO Args
getArgsWith = cmdArgs . annotate

getArgs :: IO Args
getArgs = getArgsWith args

mainWith :: (Mutable a, ShowMutable a)
         => Args
         -> a -> (a -> [Property]) -> IO ()
mainWith as f ps = do
  as' <- getArgsWith as
  reportWith as' f ps

defaultMain :: (Mutable a, ShowMutable a)
            => a -> (a -> [Property]) -> IO ()
defaultMain = mainWith args
