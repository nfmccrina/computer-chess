module Lib
    ( someFunc
    ) where

--import Data.Vector.Unboxed
import Bitboard (lsb)
import Random
import Numeric (showHex)
import Magic

someFunc :: IO ()
someFunc = (findRookMagic 20 10) >>= (\m -> (putStrLn "Done."))