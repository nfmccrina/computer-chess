module Bitboard
    (
        rookMask,
        popFirstBit
    ) where

import Data.Bits
import Data.Word
import Data.Vector.Unboxed

nsRookMasks :: [Word64]
nsRookMasks = [0x0080808080808000,
    0x0040404040404000,
    0x0020202020202000,
    0x0010101010101000,
    0x0008080808080800,
    0x0004040404040400,
    0x0002020202020200,
    0x0001010101010100]

ewRookMasks :: [Word64]
ewRookMasks = [
    0x7E00000000000000,
    0x007E000000000000,
    0x00007E0000000000,
    0x0000007E00000000,
    0x000000007E000000,
    0x00000000007E0000,
    0x0000000000007E00,
    0x000000000000007E]

bitTable :: Vector Int
bitTable = fromList [63, 30, 3, 32, 25, 41, 22, 33, 15, 50, 42, 13, 11, 53, 19, 34, 61, 29, 2,
    51, 21, 43, 45, 10, 18, 47, 1, 54, 9, 57, 0, 35, 62, 31, 40, 4, 49, 5, 52,
    26, 60, 6, 23, 44, 46, 27, 56, 16, 7, 39, 48, 24, 59, 14, 12, 55, 38, 28,
    8, 20, 37, 17, 36, 8]

popFirstBit :: Word64 -> Int
popFirstBit bb = bitTable ! (fromIntegral (shift (mod (fold * 0x783a9b23) 0x100000000) (-26)))
    where
        b = xor bb (bb - 1)
        fold = xor (b .&. 0xFFFFFFFF) (shift b (-32))

getMask :: [Word64] -> Word64 -> Word64
getMask masks n = Prelude.head $ Prelude.filter (\w -> not ((w .&. n) == 0)) masks

rookMask :: Word64 -> Word64
rookMask index = getMask nsRookMasks index .|. getMask ewRookMasks index