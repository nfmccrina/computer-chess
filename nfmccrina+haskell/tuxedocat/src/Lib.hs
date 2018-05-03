module Lib
    ( someFunc
    ) where

--import Data.Vector.Unboxed
import Bitboard (lsb)
import Random
import Numeric (showHex)

someFunc :: IO ()
someFunc = print (lsb 0x2000000000000000)