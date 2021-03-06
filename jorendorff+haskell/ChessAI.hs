module ChessAI(chessAI) where

import Minimax(bestMoveWithDepthLimit)
import Data.Bits(popCount, complement, (.&.), (.|.), shiftL, shiftR)
import Chess

-- The first heuristic I attempted was this amazingly bad one:
heuristic0 g = 0

-- Then I tried this almost-as-bad heuristic: just count pieces :)
heuristic1 g = 0.05 * fromIntegral (popCount (wholeSuite $ black g) - popCount (wholeSuite $ white g))

-- Next I gave all the pieces vaules.
heuristic2 g =
  let diff = 0.00001 * fromIntegral (materialAdvantageForWhite g)
  in case whoseTurn g of
       White -> -diff
       Black -> diff

materialAdvantageForWhite g =
  let
    -- The scores here are from Wikipedia, which lists a dozen or more
    -- scoring systems to choose from. This one is credited to Hans Berliner.
    --
    -- This code is complicated slightly by our trick of representing a queen
    -- as a bishop and a rook. For speed, we do not bother eliminating queens
    -- when counting the number of bishops/rooks, but rather count queens as
    -- both bishops and rooks, and then add a bit *more*...
    total side =   100 * popCount (pawns side)
                 + 320 * popCount (knights side)
                 + 333 * popCount (bishops side)
                 + 510 * popCount (rooks side)
                 +  37 * popCount (bishops side .&. rooks side)
  in total (white g) - total (black g)

-- Now, for each piece, give its side a small bonus for the number of squares it
-- attacks, and for each friendly piece it protects.
heuristic3 g =
  let
    diff = 0.00000001 * fromIntegral (1000 * materialAdvantageForWhite g
                                      + mobilityAdvantageForWhite g)
  in case whoseTurn g of
       White -> -diff
       Black -> diff

mobilityAdvantageForWhite g =
  let
    w = white g
    b = black g
    allPieces = wholeSuite w .|. wholeSuite b

    {-# INLINE movesAndProtectsAlongRay #-}
    movesAndProtectsAlongRay shiftDir smearDir square =
      let ray = smearDir (shiftDir square)
          shadow = smearDir (shiftDir (allPieces .&. ray))
      in ray .&. complement shadow
    rookMobility square = popCount $
      movesAndProtectsAlongRay shiftE smearE square
      .|. movesAndProtectsAlongRay shiftN smearN square
      .|. movesAndProtectsAlongRay shiftW smearW square
      .|. movesAndProtectsAlongRay shiftS smearS square
    bishopMobility square = popCount $
          movesAndProtectsAlongRay shiftNE smearNE square
      .|. movesAndProtectsAlongRay shiftNW smearNW square
      .|. movesAndProtectsAlongRay shiftSW smearSW square
      .|. movesAndProtectsAlongRay shiftSE smearSE square
    knightMobility square =
      if square .&. 0xffc381818181c3ff /= 0            -- 2 3 4 4 4 4 3 2
      then if square .&. 0x8100000000000081 /= 0       -- 3 4 6 6 6 6 4 3
           then 2                                      -- 4 6 8 8 8 8 6 4
           else if square .&. 0x4281000000008142 /= 0  -- 4 6 8 8 8 8 6 4
                then 3                                 -- 4 6 8 8 8 8 6 4
                else 4                                 -- 4 6 8 8 8 8 6 4
      else if square .&. 0x00003c3c3c3c0000 /= 0       -- 3 4 6 6 6 6 4 3
           then 8                                      -- 2 3 4 4 4 4 3 2
           else 6
    mobility side =
        (sum $ map rookMobility $ splitBits $ rooks side)
      + (sum $ map bishopMobility $ splitBits $ bishops side)
      + (sum $ map knightMobility $ splitBits $ knights side)
  in mobility w - mobility b

-- Now, add some points for pawns attacking and defending other pieces.
heuristic g =
  let
    diff = 0.00000001 * fromIntegral (1000 * materialAdvantageForWhite g
                                      + 3 * mobilityAdvantageForWhite g
                                      + 2 * pawnAdvantageForWhite g)
  in case whoseTurn g of
       White -> -diff
       Black -> diff

pawnAdvantageForWhite g =
  let w = white g
      wAll = wholeSuite w
      wPawns = pawns w
      b = black g
      bAll = wholeSuite b
      bPawns = pawns b
      allPieces = wAll .|. bAll    -- cf. Seuss 1963 "Hop On Pop"
      whitePawnBonus =
          (popCount $ allPieces .&. shiftL (wPawns .&. 0x00fefefefefefefe) 7)  -- NW
        + (popCount $ allPieces .&. shiftL (wPawns .&. 0x007f7f7f7f7f7f7f) 9)  -- NE
      blackPawnBonus =
          (popCount $ allPieces .&. shiftR (bPawns .&. 0x7f7f7f7f7f7f7f00) 7)  -- SE
        + (popCount $ allPieces .&. shiftR (bPawns .&. 0xfefefefefefefe00) 9)  -- SW
  in whitePawnBonus - blackPawnBonus

chessAI = bestMoveWithDepthLimit heuristic 2
