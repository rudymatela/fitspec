module SizedTestTypes where

import Test.Types
import FitSpec

unsignedSize :: Integral a => a -> Int
unsignedSize = fromIntegral

signedSize :: Integral a => a -> Int
signedSize = fromIntegral . signedSize'
  where
    signedSize' 0 = 0
    signedSize' n | n > 0     = n*2 - 1
                  | otherwise = (abs n)*2

instance Sized UInt2 where
  size = unsignedSize

instance Sized UInt3 where
  size = unsignedSize

instance Sized Int2 where
  size = signedSize

instance Sized Int3 where
  size = signedSize
