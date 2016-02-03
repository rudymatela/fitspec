{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module FitSpec.Grey.CatchEvaluation
  ( catchE
  , catchedE
  , noE
  , throwE)
where

#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#endif
import System.IO.Unsafe
import Control.Exception
import Data.Maybe
import Data.Functor ((<$>))

#if __GLASGOW_HASKELL__ >= 710
data Evaluated = Evaluated
  deriving Show
#else
import Data.Typeable
data Evaluated = Evaluated
  deriving (Show, Typeable)
#endif

instance Exception Evaluated

catchE :: a -> Maybe a
catchE x = unsafePerformIO $
  (Just <$> evaluate x) `catch` \e -> do let _ = e :: Evaluated
                                         return Nothing

catchedE :: a -> Bool
catchedE = isNothing . catchE

noE :: a -> Bool
noE = isJust . catchE

throwE :: a
throwE = throw Evaluated
