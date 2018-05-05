{-# OPTIONS_GHC -fno-warn-overflowed-literals #-}
module Magic
    (
        findRookMagic
    ) where

import Prelude hiding (replicate)
import Bitboard
import Random
import Data.Word
import Data.Bits
import Data.Vector.Unboxed hiding ((++))
import Numeric (showHex)

findRookMagic :: Int -> Int -> IO Word64
findRookMagic square bits = getRandomNumberFewBits >>= (\magic -> ((putStrLn ("Trying " ++ (showHex magic ""))) >> return magic)) >>= (\magic ->
    if ((mask * magic) .&. 0xFF00000000000000) < 6
    then findRookMagic square bits
    else
        if verifyMagic magic bits (occupancyAndAttacks square (rookMask square)) (replicate 4096 0) 0
        then return magic
        else findRookMagic square bits)
    where
        mask = rookMask square

verifyMagic :: Word64 -> Int -> Vector (Word64, Word64) -> Vector Word64 -> Int -> Bool
verifyMagic magic bits occupancyAndAttacks used index
        | index == (shift 1 bits) = True
        | (used ! j) == 0x0 = verifyMagic magic bits occupancyAndAttacks (update used (singleton (j, snd (occupancyAndAttacks ! index)))) (index + 1)
        | (used ! j) /= snd (occupancyAndAttacks ! index) = False
        | otherwise = verifyMagic magic bits occupancyAndAttacks used (index + 1)
        where
            j = generateIndex (fst (occupancyAndAttacks ! index)) magic bits

generateIndex :: Word64 -> Word64 -> Int -> Int
generateIndex occupancy magic bits = fromIntegral (shift (occupancy * magic) (bits - 64))


