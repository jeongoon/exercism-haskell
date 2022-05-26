module Temperature (tempToC, tempToF) where

import Control.Arrow ((>>>))

offsetToF :: Fractional a => a
offsetToF = 32

ratioToF :: Fractional a => a
ratioToF = 9 / 5

tempToC :: (Integral a, Fractional b) => a -> b
tempToC = 
      fromIntegral
  >>> (+) (negate offsetToF)
  >>> (/ ratioToF)

tempToF :: (Integral a, RealFrac b) => b -> a
tempToF = 
      (*) ratioToF
  >>> (+) offsetToF
  >>> ceiling
