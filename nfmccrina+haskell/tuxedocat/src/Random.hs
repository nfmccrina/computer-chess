module Random
    (
        getRandomNumberFewBits
    ) where

import Data.Word
import Data.Bits
import System.Random

getRandomNumber :: IO Word64
getRandomNumber = newStdGen >>= (\g -> return (head (randoms g :: [Word64])))

getRandomNumberFewBitsRecursive :: Word64 -> Int -> IO Word64
getRandomNumberFewBitsRecursive n 0 = return n
getRandomNumberFewBitsRecursive n c = getRandomNumber >>=
    (\x -> getRandomNumberFewBitsRecursive (n .&. x) (c - 1))

getRandomNumberFewBits :: IO Word64
getRandomNumberFewBits = getRandomNumberFewBitsRecursive 0xFFFFFFFFFFFFFFFF 3