module Utilities where

import Numeric (showHex)
import Data.Char (toUpper)

-- Custom showHex with 0x, capital letters, padding and no "" parameter
showHexX :: (Integral a, Show a) => a -> String
showHexX n =
    let val = map toUpper (showHex n "")
        padLen = if odd (length val) then 1 else 0
        padded = replicate padLen '0' ++ val
    in "0x" ++ padded

-- Same thing but without the Ox
showHexF :: (Integral a, Show a) => a -> String
showHexF n =
    let val = map toUpper (showHex n "")
        padLen = if odd (length val) then 1 else 0
        padded = replicate padLen '0' ++ val
    in padded