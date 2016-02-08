-- | Exports "main" functions for FitSpec.
-- They work exactly by 'report' and 'reportWith' but can be customized by
-- command line arguments.
--
-- > main = mainWith args { ... } functions properties
{-# Language DeriveDataTypeable #-}
module FitSpec.Main
  ( mainWith
  , defaultMain
  , typeArgument
  , module FitSpec.Engine -- deprecated export, remove later
  )
where

import FitSpec.Engine
import System.Console.CmdArgs hiding (args)
import Control.Monad (liftM)
import FitSpec.Mutable

data Override = Override
  { mutants__             :: Maybe Int
  , ntests__              :: Maybe Int
  , seconds__             :: Maybe Int
  , limitResults__        :: Maybe Int
  , type__                :: String
  } deriving (Data,Typeable,Show,Eq)

overrideArgs = Override
  { mutants__      = Nothing &= help "number of mutant variations"
  , ntests__       = Nothing &= help "number of tests"
  , seconds__      = Nothing &= help "timeout in seconds"
  , limitResults__ = Nothing &= help "result lines"
  , type__         = ""      &= help "type to use in polymorphic instances"
  }

-- TODO: limitResults and disableExtraMutants
override :: Args a -> Override -> Args a
override a o@Override {mutants__ = Just n} = override a {nMutants = n}
                    o {mutants__ = Nothing}
override a o@Override {ntests__  = Just n} = override a {nTestsF  = const n}
                    o {ntests__  = Nothing}
override a o@Override {seconds__ = Just s} = override a {minimumTime = s}
                    o {seconds__ = Nothing}
override a o@Override {limitResults__ = Just n} = override a {limitResults = Just n}
                    o {limitResults__ = Nothing}
override a _ = a

typeArgument :: IO String
typeArgument = liftM type__ (cmdArgs overrideArgs)

mainWith :: (Mutable a, ShowMutable a)
         => Args a
         -> a -> (a -> [Property]) -> IO ()
mainWith args f props = do
  or <- cmdArgs overrideArgs
  reportWith (override args or) f props

defaultMain :: (Mutable a, ShowMutable a)
            => a -> (a -> [Property]) -> IO ()
defaultMain = mainWith args
