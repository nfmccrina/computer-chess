module Bitboard
    (
        rookMask,
        lsb,
        removeLSB,
        occupancyAndAttacks,
        slidingMovesN,
        slidingMovesS,
        slidingMovesE,
        slidingMovesW,
        rookAttacks
    ) where

import Data.Bits
import Data.Word
import Data.Vector.Unboxed

nsMasks :: [Word64]
nsMasks = [0x8080808080808080,
    0x4040404040404040,
    0x2020202020202020,
    0x1010101010101010,
    0x0808080808080808,
    0x0404040404040404,
    0x0202020202020202,
    0x0101010101010101]

ewMasks :: [Word64]
ewMasks = [
    0xFF00000000000000,
    0x00FF000000000000,
    0x0000FF0000000000,
    0x000000FF00000000,
    0x00000000FF000000,
    0x0000000000FF0000,
    0x000000000000FF00,
    0x00000000000000FF]

edgeMask :: Word64
edgeMask = 0xFF818181818181FF

topEdgeMask :: Word64
topEdgeMask = 0xFF00000000000000

bottomEdgeMask :: Word64
bottomEdgeMask = 0x00000000000000FF

leftEdgeMask :: Word64
leftEdgeMask = 0x0101010101010101

rightEdgeMask :: Word64
rightEdgeMask = 0x8080808080808080

bitTable :: Vector Int
bitTable = fromList [63, 30, 3, 32, 25, 41, 22, 33, 15, 50, 42, 13, 11, 53, 19, 34, 61, 29, 2,
    51, 21, 43, 45, 10, 18, 47, 1, 54, 9, 57, 0, 35, 62, 31, 40, 4, 49, 5, 52,
    26, 60, 6, 23, 44, 46, 27, 56, 16, 7, 39, 48, 24, 59, 14, 12, 55, 38, 28,
    8, 20, 37, 17, 36, 8]

lsb :: Word64 -> Int
lsb bb = bitTable ! (fromIntegral (shift (mod (fold * 0x783a9b23) 0x100000000) (-26)))
    where
        b = xor bb (bb - 1)
        fold = xor (b .&. 0xFFFFFFFF) (shift b (-32))

removeLSB :: Word64 -> Word64
removeLSB n = n .&. (n - 1)

getMask :: [Word64] -> Word64 -> Word64
getMask masks square = Prelude.head $ Prelude.filter (\m -> not ((m .&. square) == 0)) masks

rookMask :: Int -> Word64
rookMask square = (col .&. (xor col (topEdgeMask .|. bottomEdgeMask))) .|. (row .&. (xor row (leftEdgeMask .|. rightEdgeMask)))
        where
            col = getMask nsMasks (shift 1 square)
            row = getMask ewMasks (shift 1 square)

occupancyAndAttacks :: Int -> Word64 -> Vector (Word64, Word64)
occupancyAndAttacks square mask = fromList $ Prelude.map (getOccupancyAndAttack square mask) [0..((shift 1 (popCount mask)) - 1)]


getOccupancyAndAttack :: Int -> Word64 -> Int -> (Word64, Word64)
getOccupancyAndAttack square mask index = (b, rookAttacks square b)
    where
        b = indexToOccupancy index (popCount mask) mask

indexToOccupancy :: Int -> Int -> Word64 -> Word64
indexToOccupancy index bits mask = fst (Prelude.foldl
    (\x y ->
        if not ((index .&. (shift 1 y)) == 0)
        then ((fst x) .|. shift 1 (lsb (snd x)), removeLSB (snd x))
        else (fst x, removeLSB (snd x)))
    (0x0, mask) [0..bits])

rookAttacks :: Int -> Word64 -> Word64
rookAttacks square blockers = n .|. w .|. s .|. e
            where
                n = slidingMovesN square blockers
                s = slidingMovesS square blockers
                e = slidingMovesE square blockers
                w = slidingMovesW square blockers

slidingMovesN :: Int -> Word64 -> Word64
slidingMovesN square blockers = slidingMoves square 8 blockers (topEdgeMask .|. (shift topEdgeMask (-8)))

slidingMovesS :: Int -> Word64 -> Word64
slidingMovesS square blockers = slidingMoves square (-8) blockers (bottomEdgeMask .|. (shift bottomEdgeMask 8))

slidingMovesE :: Int -> Word64 -> Word64
slidingMovesE square blockers = slidingMoves square 1 blockers (rightEdgeMask .|. (shift rightEdgeMask (-1)))

slidingMovesW :: Int -> Word64 -> Word64
slidingMovesW square blockers = slidingMoves square (-1) blockers (leftEdgeMask .|. (shift leftEdgeMask 1))

slidingMoves :: Int -> Int -> Word64 -> Word64 -> Word64
slidingMoves square offset blockers borderMask = slidingMovesRecursive square offset blockers borderMask 0x0

slidingMovesRecursive :: Int -> Int -> Word64 -> Word64 -> Word64 -> Word64
slidingMovesRecursive square offset blockers borderMask currentMoves = if (borderMask .&. bitboardSquare) == 0 && (bitboardSquare .&. blockers) == 0 then slidingMovesRecursive (square + offset) offset blockers borderMask (currentMoves .|. (shift bitboardSquare offset)) else currentMoves
    where
        bitboardSquare = shift 1 square