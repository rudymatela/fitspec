module CatchEvaluation (catchE, catchedE, noE, throwE) where

import System.IO.Unsafe
import Control.Exception
import Data.Maybe

data Evaluated = Evaluated
  deriving Show

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
