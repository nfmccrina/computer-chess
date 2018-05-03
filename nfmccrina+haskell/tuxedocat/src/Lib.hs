module Lib
    ( someFunc
    ) where

--import Data.Vector.Unboxed
import Bitboard (popFirstBit)
import Random
import Numeric (showHex)

someFunc :: IO ()
someFunc = print (popFirstBit 0x0000000000000004)