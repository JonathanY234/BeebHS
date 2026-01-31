module Utilities where

import Numeric (showHex)
import Data.Char (toUpper)

-- Custom showHex with 0x, capital letters, padding and no "" parameter
-- showHexX :: (Integral a, Show a) => a -> String
-- showHexX n =
--     let val = map toUpper (showHex n "")
--         padLen = if odd (length val) then 1 else 0
--         padded = replicate padLen '0' ++ val
--     in "0x" ++ padded

showHexX :: (Integral a) => a -> String
showHexX n =
    let (sign, absVal) = if n < 0 then ('-', abs n) else ('\0', n)
        val = map toUpper (showHex absVal "")
        padLen = if odd (length val) then 1 else 0
        padded = replicate padLen '0' ++ val
        finalUnsigned = "0x" ++ padded
    in if sign == '-' then '-' : finalUnsigned else finalUnsigned

-- Same thing but without the Ox
showHexF :: (Integral a) => a -> String
showHexF n =
    let (sign, absVal) = if n < 0 then ('-', abs n) else ('\0', n)
        val = map toUpper (showHex absVal "")
        padLen = if odd (length val) then 1 else 0
        padded = replicate padLen '0' ++ val
    in if sign == '-' then '-' : padded else padded