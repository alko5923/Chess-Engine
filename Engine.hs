--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Engine (fromFEN, generateMoves, searchAndPlay, isCheckmate)
where

import Data.Char
import Debug.Trace
import BoardAndPieces
import HelperFunctions
import Data.List

{- MTree
   A general tree datatype for representing a game tree
   EXAMPLE: "MTree (gamestate1.1) [(MTree (gamestate 2.1) []), MTree (gamestate2.2) [])]" represents a gamestate with two possible continuations 
-}
data MTree a =  MTree a [MTree a] 
  deriving (Show)

----------------------------------------------------------
-- INTERFACE
----------------------------------------------------------

{- fromFEN str
   Convert a FEN string to a Board representation
   PRE: str must contain <PiecePlacement> and <SideToMove> part according to the FEN standard and represent a 64 square chess classical chess position
   RETURNS: Board with position as given in str	
   EXAMPLES: fromFEN "pppppppp/rnbqkbnr/8/8/8/8/PPPPPPPP/RNBQKBNR" ==  
    ["Out","Out","Out","Out","Out","Out","Out","Out","Out","Out",
    "Out","Out","Out","Out","Out","Out","Out","Out","Out","Out",
    "Out", Black Rook","Black Knight","Black Bishop","Black Queen","Black King","Black Bishop","Black Knight","Black Rook","Out",
    "Out","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn", "Out",
    "Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out",
    "Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out",
    "Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out",
    "Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out",
    "Out","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn",White Pawn","Out",
    "Out","White Rook","White Knight","White Bishop","White Queen","White King","White Bishop","White Knight","White Rook","Out",
    "Out","Out","Out","Out","Out","Out","Out","Out","Out","Out",
    "Out","Out","Out","Out","Out","Out","Out","Out","Out","Out"]
-}
fromFEN :: String -> Board 

{- generateMoves board
   generate all possible legal moves for one position
   PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
   RETURNS: A list of all legal moves for the active side in the position of ''board''
   EXAMPLES: generateMoves createStartPosition [] == 
              [(92,71),(92,73),(97,76),(97,78),(81,71),
               (81,61),(82,72),(82,62),(83,73),(83,63),
               (84,74),(84,64),(85,75),(85,65),(86,76),
               (86,66),(87,77),(87,67),(88,78),(88,68)] 
-}
generateMoves :: Board -> [Move] -> [Move]

{- searchAndPlay b mvs
   Computes a legal move from the position b and the game moves mvs
   PRE: Board must be a valid representation of a chess board. Please see the documentation for details.
   RETURNS: A move which is legal for the board 'b' and game moves 'mvs'
   EXAMPLES: "searchAndPlay BOARD GAMEMOVES == (UPDATED_BOARD, UPDATED_GAMEMOVES)" 
                where BOARD is the position to be searched,
                GAMEMOVES is the list of all moves so far in the game
                UPDATED_BOARD is the position after the generated move has been played
                UPDATED_GAMEMOVES is the list of all moves after the generated move has been played

                  a valid expression would be for example
                  ghci> searchAndPlay createStartBoard []
-}
searchAndPlay :: Board -> [Move] -> (Board, [Move])

{- isCheckmate board moves
   Checks if position is checkmate, ie game over
   PRE: Board must be a valid representation of a chess board. Please see the documentation for details.
   RETURNS: True if the king is in check, and there are no legal moves 
   EXAMPLE: isCheckmate (fromFEN "rnbqk2r/pppp1Qpp/5n2/2b1p3/2B1P3/8/PPPP1PPP/RNB1K1NR b") [] == True
            isCheckmate createStartBoard []                                                   == False
-}
isCheckmate :: Board -> [Move] -> Bool

------------------------------------------------------------
-- INTERNAL FUNCTIONS
------------------------------------------------------------

{- generateMoves' board
   generate all possible moves WITHOUT filtering out moves that leaves the king in check
   PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
   RETURNS: A list of possible moves that may leave the own king in check in the position of ''board'' 
   EXAMPLES: generateMoves createStartPosition [] == 
              [(92,71),(92,73),(97,76),(97,78),(81,71),
               (81,61),(82,72),(82,62),(83,73),(83,63),
               (84,74),(84,64),(85,75),(85,65),(86,76),
               (86,66),(87,77),(87,67),(88,78),(88,68)] 
-}
generateMoves' :: Board -> [Move] -> [Move]

{- searchSquare (i,s) b
   Search for all pieces (excluding pawns) that can reach a given square
   PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
   RETURNS: Index of all of the active players pieces that can reach the given square (excluding pawns)
   EXAMPLES: searchSquare (75, Empty) createStartBoard == []
             searchSquare (71, Empty) createStartBoard == [92]
-}
searchSquare :: (Index, Square) -> Board -> [Index]

{- squareAttacked b s
   Checks if a piece is attacked from an enemy piece
   PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
   RETURNS: True if square 's' is in reach of an enemy piece to capture 
   EXAMPLES: searchSquare createStartBoard(41, Empty)  == True
             searchSquare createStartBoard(51, Empty)  == False
   NOTE: This function does not consider wheather the attacker is pinned or not, nor should it since it is fully legal for a pinned piece to deliver check.
-}
squareAttacked :: Board -> (Index, Square) -> Bool

{- isCheck board
   Checks if the active player is under check
   PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
   RETURNS: True if the active players king is in check 
   EXAMPLES: isCheck createStartBoard == False
             isCheck (fromFEN "rnbqkbnr/ppp2ppp/3p4/1B2p3/3PP3/8/PPP2PPP/RNBQK1NR b") == True
             isCheck (fromFEN "rnbqkbnr/ppp2ppp/3p4/1B2p3/3PP3/8/PPP2PPP/RNBQK1NR w") == False
-}
isCheck :: Board -> Bool

{- validMove board moves move
   Check if a move is valid in given position
   PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
   RETURNS: True if 'move' is legal given the position 'board' and game moves 'moves'
   EXAMPLES: validMove createStartBoard [] (21,22)  == False
             validMove createStartBoard [] (85, 65) == True
-}
validMove :: Board -> [Move] -> Move -> Bool

{- minMax Mtree
   Returns the move in MTree with that would lead to the highest posistion evaluation
   RETURNS: The move that lead to the highest position evaluation, given equally good counter moves
   EXAMPLES: minMax [MTree START] == ((NEW_BOARD, [NEW_MOVES]), SCORE)
                where [MTree START] is the list of trees generated from the starting posotion
                and NEW_BOARD and [NEW_MOVES] are the result of performing the best move found
   NOTE: minMAx and maxMin are mutually recursive, meaning they compute the result by alternating calls 
          to each other. 
   VARIANT: Height Mtree - 1
-}
minMax :: [MTree ((Board, [Move]), Int)] -> ((Board, [Move]), Int)

{- maxMin Mtree
   Returns the move in MTree with that would lead to the lowest posistion evaluation
   RETURNS: The move that lead to the lowest position evaluation, given equally good counter moves
   EXAMPLES: maxMin [MTree START] == ((NEW_BOARD, [NEW_MOVES]), SCORE)
                where [MTree START] is the list of trees generated from the starting posotion
                and NEW_BOARD and [NEW_MOVES] are the result of performing the best move found
                i.e. the best move for the minimizing player means the LOWEST evaluation
   NOTE: minMAx and maxMin are mutually recursive, meaning they compute the result by alternating calls 
          to each other. 
   VARIANT: Height Mtree - 1
-}
maxMin :: [MTree ((Board, [Move]), Int)] -> ((Board, [Move]), Int)

{- generateMTree b mvs n
   Generates a game tree down to a certain depth
   RETURNS: A (MTree ((Board, [Move]), Int) representing the positions of all legal variants (n+1) moves forward
   EXAMPLES: generateMTree createStartBoard [] 1 == [(M1 _ [M1.1, M1.2 ... ]), (M2 _ [M2.1, M2.2 ...]), ...]
                i.e. a list of MTrees representing all possible positions that can occur after
                one move by white, followed by one move by black 
-} 
generateMTree :: Board -> [Move] -> Int -> [MTree ((Board, [Move]), Int)]

{- growMTree trees
   generates a new forest from trees where the height has been increased by 1
   RETURNS: A list of trees one level higher than the input trees
   EXAMPLES: growMTree forest == NEW_FOREST
                where NEW_FOREST is forest with new subtrees generated for each leaf
-}
growMTree :: [MTree ((Board, [Move]), Int)] -> [MTree ((Board, [Move]), Int)]

{- createForest board gamemvs genmvs
   Creates a forest, i.e. a list of (MTree ((Board, [Move]), Int) from a given position and a list
      generated moves.
   RETURNS: A list of MTrees representing all moves in genmvs
   EXAMPLES: createForest createStartBoard [] [GEN_MOVES] == [M1, M2,..., Mn]
                where M1, M2,..., Mn are MTrees corresponding to one move in [GEN_MOVES] 
-}
createForest :: Board -> [Move] -> [Move] -> [MTree ((Board, [Move]), Int)]

{-  numberMovesSode board color
    Returns numbers of moves a side has in given position. 
    PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
    RETURNS: Number of moves for given color on board. 
    EXAMPLES: numberMovesSide (createStartBoard) White == 20 
              numberMovesSide (createStartBoard) Black == 20  
-}
numberMovesSide :: Board -> Color -> Int 

{-  evaluatePosition board
    Evaluates the material and move advantage for given side.
    PRE: board must be a valid representation of a chess board. Please see to the documentation for details.
    RETURNS:    an Int that represent the difference in material + difference in number of possible moves
                on board for given color.
    EXAMPLES:   evaluatePositionSide (createStartBoard) White == 0 
                evaluatePositionSide (fromFEN "8/7k/4r3/8/7K/8/3PP3/8 w") White == -300
                countMaterial (fromFEN "8/7k/4r3/8/7K/8/3PP3/8 w") Black == 300 
-}
evaluatePosition :: Board -> Int 

----------------------------------------------------------
-- IMPLEMENTATION
----------------------------------------------------------

-- Utility function that extracts a pure value from a Maybe type
-- EXAMPLE: fromJust (Just 7) = 7
fromJust (Just a ) = a
fromJust Nothing = error "Nothing error"

generateMoves b moves = filter legal $ generateMoves' b moves
  where
    legal = not . isCheck . changeTurnColor . fromJust . (move b)

generateMoves' b moves = genPieces b ++ genPawns b ++ genCastles b moves
--To generate moves for the pieces we iterate through all squares that are either empty or occupied by an enemy piece
--For each of these squares we imagine a "super-piece" that can move like combination of Queen and Knight
--We let our super-piece travel in all possible directions until we either:
--    i) hit an enemy piece, in which case we start over and search in the next direction
--   ii) move outside the board, in which case we start over and search in the next direction
--  iii) hit one of our own pieces, in which case we record that this piece can move to our starting square if the direction is valid for this piece type
--
-- NOTE: This function generates pseudo-legal moves, meaning it does not account for check, checkmate, stalemate, 50 move draw etc. This validation fall
-- into the callers responsibility. 
  where
    genPieces ([], _) = []
    genPieces (((_, Out):xs), sideToMove) = genPieces (xs, sideToMove)
    genPieces ((x@(i, Ps piece):xs), sideToMove) = 
      if c piece == sideToMove 
        then genPieces (xs, sideToMove) --square occupied by players own piece, and is hence unavailable 
        else (zip (searchSquare x b) (repeat i)) ++ genPieces (xs, sideToMove)
    genPieces ((x@(i, Empty):xs), sideToMove) = 
      (zip (searchSquare x b) (repeat i)) ++ genPieces (xs, sideToMove)
    genPieces ((x@(i, squareError):xs), sideToMove) = error "squareError found in board" 

    genPawns ([], _) = []
    genPawns (((_, Out):xs), sideToMove) = genPawns (xs, sideToMove)
    genPawns ((x@(i, Ps piece):xs), sideToMove) = 
      if t piece == Pawn && c piece == sideToMove
        then (zip (repeat i) (pawnForward i piece sideToMove)) ++ 
             (zip (repeat i) (pawnCapture i piece sideToMove)) ++ 
             (zip (repeat i) (checkEp i sideToMove)) ++ 
             genPawns (xs, sideToMove)
        else genPawns (xs, sideToMove)
        where
          pawnForward i piece White 
            | i > 80 && i < 89 = -- Pawns on the starting squares 
              let (i', s, i'', s') = ((i-10), (boardLookupSquare b (i-10)), (i-20), (boardLookupSquare b (i-20))) in 
                if s == Empty
                  then if s' == Empty then [i', i''] else [i']
                  else [] 
            | i > 30 && i < 39       =
--HANDLE WHITE PROMOTION HERE!!!
              let (_, s) = ((i-10), boardLookupSquare b (i-10)) in
                if s == Empty then [12000+i-10, 12100+i-10, 12200+i-10, 12300+i-10] else [] 
            | otherwise       =
              let (_, s) = ((i-10), boardLookupSquare b (i-10)) in
                if s == Empty then [i-10] else [] 
          pawnForward i piece Black 
            | i > 30 && i < 39 = --Pawns on the starting squares 
              let (i', s, i'', s') = ((i+10), (boardLookupSquare b (i+10)), (i+20), (boardLookupSquare b (i+20))) in 
                if s == Empty
                  then if s' == Empty then [i', i''] else [i']
                  else [] 
            | i > 80 && i < 89       =
--HANDLE BLACK PROMOTION HERE!!!
              let (_, s) = ((i-10), boardLookupSquare b (i+10)) in
                if s == Empty then [i+10] else [] 
            | otherwise       =
              let (_, s) = ((i+10), boardLookupSquare b (i+10)) in
                if s == Empty then [i+10] else []
      
          pawnCapture i piece White = 
            let moves' = [((i-9), boardLookupSquare b (i-9))] ++ [((i-11), boardLookupSquare b (i-11))] in
              concat $ map captureAvailable moves'
          pawnCapture i piece Black = 
            let moves' = [((i+9), boardLookupSquare b (i+9))] ++ [((i+11), boardLookupSquare b (i+11))] in
              concat $ map captureAvailable moves'

          --Compute possible en passant moves
          checkEp i color 
            | (color == White && i > 50 && i < 59) || (color == Black && i > 60 && i < 69) = 
            let
              searchEnemyPawn (i', Ps piece) = if color == White
                                                 then t piece == Pawn && c piece == Black && checkMv i' color 
                                                 else t piece == Pawn && c piece == White && checkMv i' color 
              searchEnemyPawn _             = False 
              checkMv i'' White = if moves == [] then False else i'' == snd (last moves)
              checkMv i'' Black = if moves == [] then False else i'' == snd (last moves) in
                if searchEnemyPawn ((i+1), boardLookupSquare b (i+1))
                  then if color == White
                         then [11000+i-9]
                         else [11100+i+11]
                  else if searchEnemyPawn ((i-1), boardLookupSquare b (i-1))
                    then if color == White 
                           then [11000+i-11] 
                           else [11100+i+9] 
                    else []
            | otherwise = []

          captureAvailable (i, Empty) = []
          captureAvailable (i, Ps piece) = if c piece /= sideToMove then [i] else []
          captureAvailable _ = []

    genPawns (((_, Empty):xs), sideToMove) = genPawns (xs, sideToMove)
    genPawns (((i, squareError):xs), _) = error "squareError found in Board"

    genCastles (board, sideToMove) moves = if sideToMove == White
                                            then wkCastles ++ wqCastles
                                            else bkCastles ++ bqCastles 
      where 
        wkCastles = if kSideS == Empty && not (squareAttacked b (kingIndex +1, Empty)) && 
                       kSideS' == Empty && not (squareAttacked b (kingIndex +2, Empty)) && 
                       findRook (kingIndex + 3) && 
                       not (piecesMoved 0) then [(kingIndex, kingIndex+2)] else []
        wqCastles = if qSideS == Empty && not (squareAttacked b (kingIndex -1, Empty)) && 
                       qSideS' == Empty && not (squareAttacked b (kingIndex -2, Empty)) && 
                       qSideS'' == Empty && not (squareAttacked b (kingIndex -4, Empty)) && 
                       findRook (kingIndex - 4) && 
                       not (piecesMoved 1) then [(kingIndex, kingIndex-2)] else []
        bkCastles = if kSideS == Empty && not (squareAttacked b (kingIndex +1, Empty)) && 
                       kSideS' == Empty && not (squareAttacked b (kingIndex +2, Empty)) && 
                       findRook (kingIndex + 3) && 
                       not (piecesMoved 0) then [(kingIndex, kingIndex+2)] else []
        bqCastles = if qSideS == Empty && not (squareAttacked b (kingIndex -1, Empty)) && 
                       qSideS' == Empty && not (squareAttacked b (kingIndex -2, Empty)) && 
                       qSideS'' == Empty && not (squareAttacked b (kingIndex -3, Empty)) && 
                       findRook (kingIndex - 4) && 
                       not (piecesMoved 1) then [(kingIndex, kingIndex-2)] else []
        kingIndex = fst $ head $ filter findKing board 
        findKing (i, Ps piece) = c piece == sideToMove && t piece == King
        findKing _             = False
        (kSideS, kSideS') = 
          ((boardLookupSquare b (kingIndex+1)), (boardLookupSquare b (kingIndex+2))) --Get the square to the right of the king
        (qSideS, qSideS', qSideS'') = 
          ((boardLookupSquare b (kingIndex-1)), (boardLookupSquare b (kingIndex-2)), (boardLookupSquare b (kingIndex-3)))
        piecesMoved side            = -- Check if the King and Rook has moved
          if side == 0 
              then if sideToMove == White
                      then (95 `elem` moves') || (98 `elem` moves')-- White Kingside castling 
                      else (25 `elem` moves') || (28 `elem` moves')-- Black Kingside castling
              else if sideToMove == White
                    then (95 `elem` moves') || (91 `elem` moves')-- Queenside castling
                    else (25 `elem` moves') || (21 `elem` moves')-- Queenside castling
        findRook n = findRookHelper (boardLookupSquare b n)
                        where
                          findRookHelper (Ps s) = t s == Rook && c s == sideToMove 
                          findRookHelper _      = False
        moves' = map fst moves

--This is where we search for all pieces (including king, but not pawns) that can reach a given square.
searchSquare x searchBoard@(pos, sideToMove) = search' x 8 True ++ 
                             search' x 7 True ++
                             search' x 6 True ++
                             search' x 5 True ++
                             search' x 4 True ++
                             search' x 3 True ++
                             search' x 2 True ++
                             search' x 1 True ++
                             search' x 0 True ++
                             search' x (-1) True
  where
    search' (i, (Ps piece)) dir restart = if c piece == sideToMove && validDir dir piece 
                                                      then [i] 
                                                      else if restart
                                                            then search' (i, Empty) dir False
                                                            else [] 
    search' (i, Out) _ _                           = []
    search' (i, Empty) 8 _ =      --Forward movement 
      let nextSquare = boardLookupSquare searchBoard (i+10) in 
        search' ((i+10), nextSquare) 8 False
    search' (i, Empty) 7 _ =      --Forward/right movement 
      let nextSquare = boardLookupSquare searchBoard (i+9) in 
        search' ((i+9), nextSquare) 7 False
    search' (i, Empty) 6 _ =      --Right movement 
      let nextSquare = boardLookupSquare searchBoard (i-1) in 
        search' ((i-1), nextSquare) 6 False
    search' (i, Empty) 5 _ =      --Down/right movement 
      let nextSquare = boardLookupSquare searchBoard (i-11) in 
        search' ((i-11), nextSquare) 5 False
    search' (i, Empty) 4 _ =      --Down movement 
      let nextSquare = boardLookupSquare searchBoard (i-10) in 
        search' ((i-10), nextSquare) 4 False
    search' (i, Empty) 3 _ =      --Down/Left movement 
      let nextSquare = boardLookupSquare searchBoard (i-9) in 
        search' ((i-9), nextSquare) 3 False
    search' (i, Empty) 2 _ =      --Left movement 
      let nextSquare = boardLookupSquare searchBoard (i+1) in 
        search' ((i+1), nextSquare) 2 False
    search' (i, Empty) 1 _ =      --Left/Up movement 
      let nextSquare = boardLookupSquare searchBoard (i+11) in 
        search' ((i+11), nextSquare) 1 False
    search' (i, Empty) 0 _ =      -- Knight movement
      let nextSquares = map (\x -> (,) x (boardLookupSquare searchBoard x)) [i+21, i+19, i+12, i+8, i-21, i-19, i-12, i-8] in 
        concat $ map (\x -> knightSearch x sideToMove) nextSquares
          where
            knightSearch (i, (Ps piece)) color = if c piece == color && t piece == Knight
                                                   then [i]
                                                   else []
            knightSearch _ _                   = []
    search' (i, Empty) (-1) _ =     -- King movement 
      let nextSquares = map (\x -> (,) x (boardLookupSquare searchBoard x)) [i+11, i+10, i+9, i+1, i-1, i-9, i-10, i-11] in
            concat $ map (\x -> kingSearch x sideToMove) nextSquares
        where
          kingSearch (i, (Ps piece)) color = if c piece == color && t piece == King
                                                then [i]
                                                else []
          kingSearch _ _                   = []

    --Check if motion is legal for a piece type
    validDir 8 piece = t piece == Rook || t piece == Queen 
    validDir 7 piece = t piece == Bishop || t piece == Queen
    validDir 6 piece = t piece == Rook || t piece == Queen 
    validDir 5 piece = t piece == Bishop || t piece == Queen
    validDir 4 piece = t piece == Rook || t piece == Queen 
    validDir 3 piece = t piece == Bishop || t piece == Queen
    validDir 2 piece = t piece == Rook || t piece == Queen 
    validDir 1 piece = t piece == Bishop || t piece == Queen
    validDir 0 piece = t piece == Knight
    validDir _ _ = error "invalid direction"

--Test functions, remove later
--EXAMPLE: testGen "2R1r3/n4ppp/2Q5/4P3/8/8/8/8 w" returns all white moves (excluding pawn and king for now) in the given position
testGen fen moves = map (\(a,b) -> indexToSquare a ++ indexToSquare b) $ generateMoves (fromFEN fen) moves

isPawn col (Ps pce)   = if t pce == Pawn && c pce == col then True else False 
isPawn _ _            = False 

squareAttacked board@(b, color) (i, piece) = 
    if color == White
      then isPawn Black (boardLookupSquare board (i-9)) ||
           isPawn Black (boardLookupSquare  board (i-11)) ||
           (not $ [] ==  searchSquare (i, Empty) (b, Black))
      else isPawn White (boardLookupSquare board (i+9)) ||
           isPawn White (boardLookupSquare board (i+11)) ||
           (not $ [] ==  searchSquare (i, Empty) (b, White)) 
                                --then elem i $ map snd $ generateMoves' (b, Black) []
                                --else elem i $ map snd $ generateMoves' (b, White) [] 

isCheck (b, color) = squareAttacked (b, color) (kingIndex b)
  where
    kingIndex []                 = error "King not found" 
    kingIndex (s@(i, Ps piece):xs) = if c piece == color && t piece == King
                                then s 
                                else kingIndex xs 
    kingIndex (_:xs)             = kingIndex xs

isCheckmate b mvs = isCheck b && length (generateMoves b mvs) == 0  

validMove b ms m = m `elem` (generateMoves b ms)

searchAndPlay b@(pos, White) ms = let ((_, ms'), _) = minMax (generateMTree b ms 1) in
                                    ((fromJust (move b (head (drop 1 ms')))), (head (drop 1 ms')):ms)
searchAndPlay b@(pos, Black) ms = let ((_, ms'), _) = maxMin (generateMTree b ms 1) in
                                    ((fromJust (move b (head (drop 1 ms')))), (head (drop 1 ms')):ms)
                                      
{-select (generateMoves b ms) in ((fromJust (move b mv)), (mv:ms)) 
  where
    select xs = fst $ listEval $ map (\x -> (x, (evaluatePositionSide (fromJust (move b x)) clr))) xs
    max' (m,e) (m',e') = if e >= e' then (m,e) else (m',e')
    listEval [x] = x
    listEval (x:xs) = max' x (listEval xs) 
-}

--Implementation of the minmax algorithm
--Most of minMax and maxMin are from this excellent post on stackoverflow:
--https://stackoverflow.com/questions/58463720/stuck-with-minimax-algorithm-what-next-haskell
minMax = maximumBy (\a b -> compare (snd a) (snd b)) . map score
  where
    score (MTree ((board, mvs), s) []) = ((board, mvs), (evaluatePosition board))
    score (MTree (b, s) forest) = maxMin forest

maxMin = minimumBy (\a b -> compare (snd a) (snd b)) . map score
  where
    score (MTree ((board, mvs), s) []) = ((board, mvs), (evaluatePosition board))
    score (MTree (board, s) forest) = minMax forest

generateMTree b mvs 0 = createForest b mvs (generateMoves b mvs)
generateMTree b mvs n = growMTree (generateMTree b mvs (n-1))

growMTree [] = []
growMTree ((MTree ((b, mvs), 0) []):xs) = (MTree ((b, mvs), 0) (createForest b mvs (generateMoves b mvs))):(growMTree xs)
growMTree ((MTree a forest):ts)    = growMTree forest ++ growMTree ts 

createForest _ _ [] = []
createForest board moves (x:xs) = (MTree ((newPos, (x:moves)), 0) []):(createForest board moves xs) 
  where
    newPos = fromJust (move board x)

fromFEN xs = (parsePos (head (words xs)), sideToMove)
  where
    sideToMove = if color == "w" then White else Black
    color = head $ drop 1 $ words xs
    parsePos = zip [0..] . addBoundries . parseFEN . mySplit
      where
        mySplit = words . map (\x -> if x == '/' then ' ' else x) --Split string at '/'
        addBoundries xs = [Out | x <- [1..20]] ++ xs ++ [Out | x <- [1..20]] --Add the outside border
        parseFEN [] = []
        parseFEN (x:xs) = [Out] ++ parseRow x ++ [Out] ++ parseFEN xs
          where
            parseRow [] = []
            parseRow (x:xs)
              | isDigit x = [Empty | x <- [1..(digitToInt x)]] ++ parseRow xs 
              | x == 'p'  = [Ps {p = Piece {c = Black, t = Pawn}}] ++ parseRow xs
              | x == 'r'  = [Ps {p = Piece {c = Black, t = Rook}}] ++ parseRow xs 
              | x == 'n'  = [Ps {p = Piece {c = Black, t = Knight}}] ++ parseRow xs 
              | x == 'b'  = [Ps {p = Piece {c = Black, t = Bishop}}] ++ parseRow xs 
              | x == 'q'  = [Ps {p = Piece {c = Black, t = Queen}}] ++ parseRow xs 
              | x == 'k'  = [Ps {p = Piece {c = Black, t = King}}] ++ parseRow xs 
              | x == 'P'  = [Ps {p = Piece {c = White, t = Pawn}}] ++ parseRow xs
              | x == 'R'  = [Ps {p = Piece {c = White, t = Rook}}] ++ parseRow xs 
              | x == 'N'  = [Ps {p = Piece {c = White, t = Knight}}] ++ parseRow xs 
              | x == 'B'  = [Ps {p = Piece {c = White, t = Bishop}}] ++ parseRow xs 
              | x == 'Q'  = [Ps {p = Piece {c = White, t = Queen}}] ++ parseRow xs 
              | x == 'K'  = [Ps {p = Piece {c = White, t = King}}] ++ parseRow xs 

numberMovesSide (pos, _) col = length $ generateMoves' (pos, col) [] 

evaluatePosition board = (countMaterial board White - countMaterial board Black)
    + (numberMovesSide board White - numberMovesSide board Black)
