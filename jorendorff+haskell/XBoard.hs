module XBoard(playXBoard, fenToBoard) where

import Control.Monad(forever)
import Control.Monad.State(StateT, liftIO, put, get, runStateT)
import System.IO(hFlush, stdout)
import System.Exit(exitWith, ExitCode(ExitSuccess))
import Data.Char(isSpace, ord)
import Data.Bits(bit, (.|.), shiftL)
import Minimax(start, moves, applyMove, scoreFinishedGame)
import Chess(Chessboard(..), Suite(..), ChessMove, ChessColor(White, Black))
import ChessAI(chessAI)

data MoveResult = MoveError String | GameOver String | Continue Chessboard

tryMove :: Chessboard -> String -> MoveResult
tryMove board str = case reads str of
  [] -> MoveError ("unrecognized move: " ++ str)
  (move, rest) : _ ->
    if not (all isSpace rest)
    then MoveError("unrecognized move: " ++ str)
    else if not (elem move (moves board))
         then MoveError ("illegal move: " ++ show move)
         else let result = applyMove board move
              in case moves result of
                   [] -> GameOver (case scoreFinishedGame result of
                                      0 -> "1/2-1/2 {stalemate}"
                                      _ -> if whoseTurn result == Black
                                           then "1-0 {White mates}"
                                           else "0-1 {Black mates}")
                   _ -> Continue result

ignoredCommands = [
  "xboard", "accepted", "rejected", "variant", "random", "white", "black", "level",
  "st", "sd", "nps", "time", "otim", "?", "ping", "draw", "result", "edit", "hint", "bk", "undo",
  "remove", "hard", "easy", "post", "nopost", "analyze", "name", "rating", "computer", "option"]

respond :: String -> StateT a IO ()
respond str = liftIO $ do
  putStrLn str
  hFlush stdout
  appendFile "xboard-input.txt" $ (concat $ map ("<-- " ++) (lines str)) ++ "\n"

say str = liftIO $ appendFile "xboard-input.txt" str

fileToInt :: Char -> Int
fileToInt file = (ord file) - (ord 'a')

fenToBoard :: String -> Chessboard
fenToBoard str = Chessboard {
  white = Suite {
    pawns   = bitBoard "P",
    knights = bitBoard "N",
    bishops = bitBoard "BQ",
    rooks   = bitBoard "RQ",
    king    = bitBoard "K",
    castleK = 'K' `elem` castlingString,
    castleQ = 'Q' `elem` castlingString},
  black = Suite {
    pawns   = bitBoard "p",
    knights = bitBoard "n",
    bishops = bitBoard "bq",
    rooks   = bitBoard "rq",
    king    = bitBoard "k",
    castleK = 'k' `elem` castlingString,
    castleQ = 'q' `elem` castlingString},
  whoseTurn = whoseTurn,
  enPassant = enPassant}
  where
    piecesStr : whoseTurnStr : castlingString : enPassantString : _ = words str

    bitBoard chars = foldr (.|.) 0
                     $ map (\(c, b) -> if c `elem` chars then b else 0)
                     $ zip array (map bit [0..63])

    array = concat $ reverse $ splitLength 8
            $ filter (/= '/') $ expandSpaces piecesStr

    expandSpaces :: String -> String
    expandSpaces (x:xs)
        | x `elem` ['1'..'8'] = (replicate (read [x]) ' ') ++ (expandSpaces xs)
        | otherwise = x:(expandSpaces xs)
    expandSpaces "" = ""

    splitLength n xs@(x:_) = take n xs : (splitLength n $ drop n xs)
    splitLength _ [] = []
    whoseTurn = case whoseTurnStr of
                  "w" -> White
                  "b" -> Black

    enPassant = case enPassantString of
      "-" -> 0
      a : _ -> shiftL pawnRow $ fileToInt a
      where
        pawnRow = case whoseTurn of
          White -> 0x0000010000000000
          Black -> 0x0000000000010000

data XBoardState = XBoardState { board :: Chessboard, forceMode :: Bool }

setForceMode x = do
  xbstate <- get
  put $ xbstate {forceMode = x}

go ai = do
  state <- get
  let move = ai $ board state
  respond $ "move " ++ (show move)
  put $ state {board = applyMove (board state) move}

commandLoop :: (Chessboard -> ChessMove) -> StateT XBoardState IO ()
commandLoop ai = forever $ do
  thisLine <- liftIO getLine
  let command = head $ words thisLine
  let arguments = drop ((length command) + 1) thisLine
  say $ "--> " ++ thisLine ++ "\n"                           
  if command `elem` ignoredCommands
  then return ()
  else case command of
    "new"      -> put $ XBoardState {board = start, forceMode = False}
    "protover" -> respond $ "feature variants=\"normal\" usermove=1 draw=0 analyze=0 colors=0 setboard=1 sigint=0 done=1"
    "quit"     -> liftIO $ exitWith ExitSuccess
    "force"    -> setForceMode True
    "setboard" -> do
      say arguments
      let boardNow = fenToBoard arguments
      state <- get
      put $ state {board = boardNow}
      say $ show boardNow
    "go" -> do
      setForceMode False
      go ai
      stateNow <- get
      say $ show $ board stateNow
    "usermove" -> do
      state <- get
      case tryMove (board state) arguments of
        MoveError msg -> respond msg
        GameOver msg -> respond msg
        Continue board' -> do
          put $ state {board = board'}
          if forceMode state then return () else go ai
      state' <- get
      say $ show $ board state'
    _ -> respond $ "Error: (unknown command): " ++ thisLine

playXBoard :: (Chessboard -> ChessMove) -> IO ()
playXBoard ai = do
  runStateT (commandLoop ai) $ XBoardState {board = start, forceMode = True}
  return ()