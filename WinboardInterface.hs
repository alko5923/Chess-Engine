import Control.Monad
import System.IO
import Engine
import BoardAndPieces
import HelperFunctions
import Data.Maybe

{- main
   The entry point of the program
   RETURNS: ()
   SIDE-EFFECTS: Performs IO operations on standard input and standard output
-}
main :: IO ()
main = communicate createStartBoard [] True

{- communicate
   The main loop which communicates with the GUI via the Chess Engine Communication Protocol
   RETURNS: ()
   SIDE-EFFECTS: Performs IO operations on standard input and standard output 
   EXAMPLE USAGE: communicate (fromFEN "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR b") [(94,63), (35,55), (85,65)]
-}
communicate :: Board -> [Move] -> Bool -> IO ()
communicate board mvs active =
  do
    cmd <- getLine
    -- "protover 2" let's us know that the GUI is ready to accept input
    when (cmd == "protover 2") $ do
      --setup commands, sigint and sigterm are are neccessary for deployment on linux
      putStrLn "feature done=0 myname=\"Check Norris\" sigint=0 sigterm=0\n"
      --"feature done=1" lets the GUI know we are done sending setup commands
      putStrLn "feature done=1 \n"  
    --Move recieved
    when (isMove cmd) $ do
      if active
        then do
          let (newBoard, newMvs) = updateBoard board mvs cmd
          if (isCheckmate newBoard newMvs)
            then do 
              endGame newBoard 
              communicate newBoard newMvs False
            else do 
              (newBoard', newMvs') <- makeMove newBoard newMvs
              communicate newBoard' newMvs' active 
        else do
          let (newBoard, newMvs) = updateBoard board mvs cmd
          communicate newBoard newMvs active
    --"go" tells the engine to start making moves for the active side
    when (cmd == "go") $ do
      if active
        then do
          communicate board mvs True
        else do
          (newBoard, newMvs) <- makeMove board mvs 
          communicate newBoard newMvs True 
    --"new" tells the engine to reset the position and start playing as black
    when (cmd == "new") $ communicate createStartBoard [] True
    --"force" tells the engine to stop making moves, but still accept moves and keep track of the position
    when (cmd == "force") $ communicate board mvs False 
    --"quit" tells the engine to immediately exit
    when (cmd == "quit") $ return ()
    hFlush stdout --neccessary to make sure commands are sent to the GUI
    communicate board mvs active

{- endGame
   Sends the command telling the GUI that the game is over by checkmate
   RETURNS: ()
   SIDE-EFFECTS: prints to stdout
-}
endGame :: Board -> IO ()
endGame (_, Black) = do
  putStrLn "1-0 {White wins}"
  return ()
endGame (_, White) = do
  putStrLn "0-1 {Black wins}"
  return ()

{- updateBoard board mvs mvStr
   Performs the move given in mvStr in the position given by board and mvs
   RETURNS: (board, mvs) whith the move mvStr performed
   EXAMPLES:"updateBoard createStartBoard [] "e2e4" == NEW_POSITION
                    where NEW_POSITION is the position resulting form moving "e2e4" on the board
 -}
updateBoard :: Board -> [Move] -> String -> (Board, [Move])
updateBoard b mvs mv = (,) (fromJust (move b (strToMove mv))) mvs 

{- makeMove board mvs
   Computes and performs a move in the game given by board and mvs
   RETURNS: (IO (board, mvs)) whith the generated move performed 
   SIDE-EFFECTS: prints to stdout
   EXAMPLES: makeMove createStartBoard [] == NEW_POSITION
                where NEW_POSITION is the position resulting from performing the generated move 
-}
makeMove :: Board -> [Move] -> IO (Board, [Move])
makeMove b mvs = do
  let (newBoard, newMvs) = searchAndPlay b mvs
  putStrLn $ "move " ++ (moveToStr (head newMvs))
  hFlush stdout
  return (newBoard, newMvs) 

{- strToMove str
   convert a string to a Move type
   PRE: str must be a valid move type string according to the Chess Engine Communication Protocol
   RETURNS: The Move corresponding to str
   EXAMPLES: strToMove "e2e4"  == (85,65)
             strToMove "c1c2Q" == (83,12393) 
-}
strToMove :: String -> Move
strToMove str@(i:i':j:j':p) | length str == 4 = (,) (squareToIndex (take 2 str)) (squareToIndex (drop 2 str))
                            | head p == 'r' = (,) (squareToIndex [i,i']) (12000 + (squareToIndex [j,j']))
                            | head p == 'n' = (,) (squareToIndex [i,i']) (12100 + (squareToIndex [j,j']))
                            | head p == 'b' = (,) (squareToIndex [i,i']) (12200 + (squareToIndex [j,j']))
                            | head p == 'q' = (,) (squareToIndex [i,i']) (12300 + (squareToIndex [j,j']))

{- moveToStr move
   convert a Move type to a string
   PRE: str must be a valid move type string according to the Chess Engine Communication Protocol
   RETURNS: The string representation of move 
   EXAMPLES: strToMove (85,65) == "e2e4" 
             strToMove (83,12393) ==  "c1c2Q"
-}
moveToStr :: Move -> String
moveToStr move@(i,j) | take 3 (show j) == "110" || 
                       take 3 (show j) == "111" = indexToSquare i ++ (indexToSquare (read (drop 3 (show j)))) 
                     | take 3 (show j) == "120" = indexToSquare i ++ (indexToSquare (read (drop 3 (show j)))) ++ "r"
                     | take 3 (show j) == "121" = indexToSquare i ++ (indexToSquare (read (drop 3 (show j)))) ++ "n"
                     | take 3 (show j) == "122" = indexToSquare i ++ (indexToSquare (read (drop 3 (show j)))) ++ "b"
                     | take 3 (show j) == "123" = indexToSquare i ++ (indexToSquare (read (drop 3 (show j)))) ++ "q"
                     | otherwise = indexToSquare i ++ indexToSquare j 

