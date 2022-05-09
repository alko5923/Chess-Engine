{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module BoardAndPieces where
import Debug.Trace

data Color      = White | Black deriving (Show,Eq)
data PieceType  = Pawn | Rook | Knight | Bishop | King | Queen deriving (Show,Eq)
data Piece      = Piece {c::Color, t::PieceType} deriving (Show,Eq)
data Square     = Empty | Out |Ps {p::Piece} | SquareError deriving (Show,Eq)

type Index        = Int
type BoardSquares = [(Index,Square)]
type Board        = ([(Index,Square)],Color)
type EngineString = [String]
type Move         = (Index,Index)
type ListOfMoves  = [Move]

{-
- CreateStartBoard
- CreateStartBoard creates a chessboard whith the pieces in starting posission
- RETURNS: a Board with pieces in starting posission and whites turn
- EXAMPLE:
          createStartBoard 
      ==> ([(0,Out),(1,Out),(2,Out),(3,Out),(4,Out),(5,Out),(6,Out),(7,Out),(8,Out),(9,Out),(10,Out),(11,Out),(12,Out),(13,Out),(14,Out),(15,Out),(16,Out),(17,Out),(18,Out),(19,Out),(20,Out),(21,Ps {p = Piece {c = Black, t = Rook}}),(22,Ps {p = Piece {c = Black, t = Knight}}),(23,Ps {p = Piece {c = Black, t = Bishop}}),(24,Ps {p = Piece {c = Black, t = King}}),(25,Ps {p = Piece {c = Black, t = Queen}}),(26,Ps {p = Piece {c = Black, t = Bishop}}),(27,Ps {p = Piece {c = Black, t = Knight}}),(28,Ps {p = Piece {c = Black, t = Rook}}),(29,Out),(30,Out),(31,Ps {p = Piece {c = Black, t = Pawn}}),(32,Ps {p = Piece {c = Black, t = Pawn}}),(33,Ps {p = Piece {c = Black, t = Pawn}}),(34,Ps {p = Piece {c = Black, t = Pawn}}),(35,Ps {p = Piece {c = Black, t = Pawn}}),(36,Ps {p = Piece {c = Black, t = Pawn}}),(37,Ps {p = Piece {c = Black, t = Pawn}}),(38,Ps {p = Piece {c = Black, t = Pawn}}),(39,Out),(40,Out),(41,Empty),(42,Empty),(43,Empty),(44,Empty),(45,Empty),(46,Empty),(47,Empty),(48,Empty),(49,Out),(50,Out),(51,Empty),(52,Empty),(53,Empty),(54,Empty),(55,Empty),(56,Empty),(57,Empty),(58,Empty),(59,Out),(60,Out),(61,Empty),(62,Empty),(63,Empty),(64,Empty),(65,Empty),(66,Empty),(67,Empty),(68,Empty),(69,Out),(70,Out),(71,Empty),(72,Empty),(73,Empty),(74,Empty),(75,Empty),(76,Empty),(77,Empty),(78,Empty),(79,Out),(80,Out),(81,Ps {p = Piece {c = White, t = Pawn}}),(82,Ps {p = Piece {c = White, t = Pawn}}),(83,Ps {p = Piece {c = White, t = Pawn}}),(84,Ps {p = Piece {c = White, t = Pawn}}),(85,Ps {p = Piece {c = White, t = Pawn}}),(86,Ps {p = Piece {c = White, t = Pawn}}),(87,Ps {p = Piece {c = White, t = Pawn}}),(88,Ps {p = Piece {c = White, t = Pawn}}),(89,Out),(90,Out),(91,Ps {p = Piece {c = White, t = Rook}}),(92,Ps {p = Piece {c = White, t = Knight}}),(93,Ps {p = Piece {c = White, t = Bishop}}),(94,Ps {p = Piece {c = White, t = King}}),(95,Ps {p = Piece {c = White, t = Queen}}),(96,Ps {p = Piece {c = White, t = Bishop}}),(97,Ps {p = Piece {c = White, t = Knight}}),(98,Ps {p = Piece {c = White, t = Rook}}),(99,Out),(100,Out),(101,Out),(102,Out),(103,Out),(104,Out),(105,Out),(106,Out),(107,Out),(108,Out),(109,Out),(110,Out),(111,Out),(112,Out),(113,Out),(114,Out),(115,Out),(116,Out),(117,Out),(118,Out),(119,Out)],White)
-}

createStartBoard :: Board
createStartBoard = csb 0 []
  where
    csb :: Index -> [(Index,Square)] -> Board
    csb 120  accB            = (accB,White)
    csb accA accB
      | elem accA [0..19]    = csb (accA+1) $ accB++[(,) accA $ Out]
      | elem accA [100..119] = csb (accA+1) $ accB++[(,) accA $ Out] 
      | (mod accA 10) == 0   = csb (accA+1) $ accB++[(,) accA $ Out]
      | (mod accA 10) == 9   = csb (accA+1) $ accB++[(,) accA $ Out]
      | elem accA [40..79]   = csb (accA+1) $ accB++[(,) accA $ Empty]
      | elem accA [31..38]   = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece Black Pawn]
      | elem accA [21,28]    = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece Black Rook] 
      | elem accA [22,27]    = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece Black Knight]
      | elem accA [23,26]    = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece Black Bishop]
      | elem accA [24]       = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece Black Queen]
      | elem accA [25]       = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece Black King]
      | elem accA [81..88]   = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece White Pawn]
      | elem accA [91,98]    = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece White Rook]
      | elem accA [92,97]    = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece White Knight]
      | elem accA [93,96]    = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece White Bishop]
      | elem accA [94]       = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece White Queen]
      | elem accA [95]       = csb (accA+1) $ accB++[(,) accA $ Ps $ Piece White King]
      | accA < 0             = ([(-1 ,SquareError)],White)
      | accA > 120           = ([(121,SquareError)],White)
      | otherwise            = ([(0  ,SquareError)],White)

{-
- fromBoardToString board
- fromBoradToString takes a board and transforms it into a EngineString
- RETURNS: a EngineString based on board
- EXAMPLES:
            fromBoardToString ([(0,Out),(1,Out),(2,Out),(3,Out),(4,Out),(5,Out),(6,Out),(7,Out),(8,Out),(9,Out),(10,Out),(11,Out),(12,Out),(13,Out),(14,Out),(15,Out),(16,Out),(17,Out),(18,Out),(19,Out),(20,Out),(21,Ps {p = Piece {c = Black, t = Rook}}),(22,Ps {p = Piece {c = Black, t = Knight}}),(23,Ps {p = Piece {c = Black, t = Bishop}}),(24,Ps {p = Piece {c = Black, t = King}}),(25,Ps {p = Piece {c = Black, t = Queen}}),(26,Ps {p = Piece {c = Black, t = Bishop}}),(27,Ps {p = Piece {c = Black, t = Knight}}),(28,Ps {p = Piece {c = Black, t = Rook}}),(29,Out),(30,Out),(31,Ps {p = Piece {c = Black, t = Pawn}}),(32,Ps {p = Piece {c = Black, t = Pawn}}),(33,Ps {p = Piece {c = Black, t = Pawn}}),(34,Ps {p = Piece {c = Black, t = Pawn}}),(35,Ps {p = Piece {c = Black, t = Pawn}}),(36,Ps {p = Piece {c = Black, t = Pawn}}),(37,Ps {p = Piece {c = Black, t = Pawn}}),(38,Ps {p = Piece {c = Black, t = Pawn}}),(39,Out),(40,Out),(41,Empty),(42,Empty),(43,Empty),(44,Empty),(45,Empty),(46,Empty),(47,Empty),(48,Empty),(49,Out),(50,Out),(51,Empty),(52,Empty),(53,Empty),(54,Empty),(55,Empty),(56,Empty),(57,Empty),(58,Empty),(59,Out),(60,Out),(61,Empty),(62,Empty),(63,Empty),(64,Empty),(65,Empty),(66,Empty),(67,Empty),(68,Empty),(69,Out),(70,Out),(71,Empty),(72,Empty),(73,Empty),(74,Empty),(75,Empty),(76,Empty),(77,Empty),(78,Empty),(79,Out),(80,Out),(81,Ps {p = Piece {c = White, t = Pawn}}),(82,Ps {p = Piece {c = White, t = Pawn}}),(83,Ps {p = Piece {c = White, t = Pawn}}),(84,Ps {p = Piece {c = White, t = Pawn}}),(85,Ps {p = Piece {c = White, t = Pawn}}),(86,Ps {p = Piece {c = White, t = Pawn}}),(87,Ps {p = Piece {c = White, t = Pawn}}),(88,Ps {p = Piece {c = White, t = Pawn}}),(89,Out),(90,Out),(91,Ps {p = Piece {c = White, t = Rook}}),(92,Ps {p = Piece {c = White, t = Knight}}),(93,Ps {p = Piece {c = White, t = Bishop}}),(94,Ps {p = Piece {c = White, t = King}}),(95,Ps {p = Piece {c = White, t = Queen}}),(96,Ps {p = Piece {c = White, t = Bishop}}),(97,Ps {p = Piece {c = White, t = Knight}}),(98,Ps {p = Piece {c = White, t = Rook}}),(99,Out),(100,Out),(101,Out),(102,Out),(103,Out),(104,Out),(105,Out),(106,Out),(107,Out),(108,Out),(109,Out),(110,Out),(111,Out),(112,Out),(113,Out),(114,Out),(115,Out),(116,Out),(117,Out),(118,Out),(119,Out)],White)
         ==>["Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Black Rook","Black Knight","Black Bishop","Black King","Black Queen","Black Bishop","Black Knight","Black Rook","Out","Out","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Black Pawn","Out","Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out","Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out","Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out","Out","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Empty","Out","Out","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn","White Pawn","Out","Out","White Rook","White Knight","White Bishop","White King","White Queen","White Bishop","White Knight","White Rook","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out","Out"]
-}
fromBoardToString :: Board -> EngineString
fromBoardToString (b,_) = fromBoardToString' b
  where
    fromBoardToString' :: [(Index,Square)] -> EngineString
    fromBoardToString' []       = []
    fromBoardToString' ((_,Empty):xs) = "Empty":(fromBoardToString' xs)
    fromBoardToString' ((_,Out):xs)   = "Out":(fromBoardToString' xs)
    fromBoardToString' ((_,Ps (Piece c t)):xs)
      | c == Black && t == Pawn   = "Black Pawn":(fromBoardToString' xs)
      | c == Black && t == Rook   = "Black Rook":(fromBoardToString' xs)
      | c == Black && t == Knight = "Black Knight":(fromBoardToString' xs)
      | c == Black && t == Bishop = "Black Bishop":(fromBoardToString' xs)
      | c == Black && t == King   = "Black King":(fromBoardToString' xs)
      | c == Black && t == Queen  = "Black Queen":(fromBoardToString' xs)
      | c == White && t == Pawn   = "White Pawn":(fromBoardToString' xs)
      | c == White && t == Rook   = "White Rook":(fromBoardToString' xs)
      | c == White && t == Knight = "White Knight":(fromBoardToString' xs)
      | c == White && t == Bishop = "White Bishop":(fromBoardToString' xs)
      | c == White && t == King   = "White King":(fromBoardToString' xs)
      | c == White && t == Queen  = "White Queen":(fromBoardToString' xs)
      | otherwise                            = "Error, unknown error in fromBoardToString":[]
    fromBoardToString' ((-1,SquareError):_)  = "Error, #-1 SfunctionquareError":[]
    fromBoardToString' ((121,SquareError):_) = "Error, #121 SquareError":[]
    fromBoardToString' ((0,SquareError):_)   = "Error, #0 SquareError":[]
    fromBoardToString' ((_,SquareError):_)   = "Error, unknown SquareError":[]

{-
- takeSquare board index
- takeSquare retruns a square from a board at a specific index
- RETURNS: a Square from a Board at a index
-}

takeSquare :: Board -> Index -> Square
takeSquare (b,_) i = snd $ b !! i

{-
- changeTurnColor (board,color)
- changeTurnColor changes the turn color for board, if white -> black, if black -> white
- RETURNS: (board, changeColor)
-}
changeTurnColor :: Board -> Board
changeTurnColor (b,White) = (b,Black)
changeTurnColor (b,Black) = (b,White)

{-
- move board wantedMove
- move take a Board and a wanted Move, checks if its valid
       - if valid -> moves the piece at a index to a diffrent piece and leves the first index Empty and returns a Board with moved piece and changed trun Color
       - if invalid -> Nothing
- PRE: the squares at the indexs in wantedMove are not type Out
- RETURNS: Board with moved piece and changed turnColor

-}
move :: Board -> Move -> Maybe Board
move b (i,i') =
       if isCastling (takeSquare b i) (i,i') 
          then Just $ moveCastling (changeTurnColor b) (i,i')
          else if isPassant (i,i')   
                then Just $ movePassant b (i,i')
                else if isPromotion (i,i') 
                      then Just $ makePromotion b (i,i')
                      else Just $ move' (changeTurnColor b) (i,i') (takeSquare b i) []
  where
    {-
    - move' moves the piece at i to i' and returns a Board
    -}
    move' :: Board -> Move -> Square -> BoardSquares -> Board
    move' ([],c) _ _ _ = ([(0,SquareError)],c)
    move' (((id,squ):xs),c) (i,i') p b'
      | id == i'  = move'' ((b'++[(i',p)]++xs),c) (i,i') []
      | otherwise = move' (xs,c) (i,i') p (b'++[(id,squ)])
    {-
    - move'' sets the Square at i to be empty and retruns a board 
    -}
    move'' :: Board -> Move -> BoardSquares -> Board
    move'' ([],c) _ _ = ([(0,SquareError)],c)
    move'' (((id,squ):xs),c) (i,i') b'
      | id == i   = ((b'++[(i,Empty)]++xs),c)
      | otherwise = move'' (xs,c) (i,i') (b'++[(id,squ)])
    {-
    - Checks if move '(i,i')' is a castling move
    -}
    isCastling :: Square -> Move -> Bool
    isCastling Out _         = False
    isCastling Empty _       = False
    isCastling SquareError _ = False
    isCastling (Ps (Piece c t)) m@(i,i')
      | t == King && (m==(25,23)||m==(25,27)||m==(95,93)||m==(95,97))= True
      | otherwise = False
    {-
    - Makes a castling move
    -}
    moveCastling :: Board -> Move -> Board
    moveCastling b (25,23) = move' (move' b (25,23) (takeSquare b 25) []) (21,24) (takeSquare b 21) []
    moveCastling b (25,27) = move' (move' b (25,27) (takeSquare b 25) []) (28,26) (takeSquare b 28) []
    moveCastling b (95,93) = move' (move' b (95,93) (takeSquare b 95) []) (91,94) (takeSquare b 91) []
    moveCastling b (95,97) = move' (move' b (95,97) (takeSquare b 95) []) (98,96) (takeSquare b 98) []
    moveCastling b (_,_)   = ([(0,SquareError)],White)
    {-
    - Checks if move '(i,i')' is a passant move
    -}
    isPassant :: Move -> Bool
    isPassant (i,i')
      | take 3 (show i') == "110" = True
      | take 3 (show i') == "111" = True
      | otherwise                 = False
    {-
    - Makes a passant move
    -}
    movePassant :: Board -> Move -> Board
    movePassant b (i,i')
      | take 3 (show i') == "110" = let i'' = read (drop 3 (show i')) in 
                                        move'' (move' (changeTurnColor b) (i,i'') (takeSquare b i) []) ((i''+10),(i''+10)) []
      | otherwise                 = let i'' = read (drop 3 (show i')) in
                                        move'' (move' (changeTurnColor b) (i,i'') (takeSquare b i) []) ((i''-10),(i''-10)) []
    {-
    - Checks if move '(i,i')' is a promotion
    -} 
    isPromotion :: Move -> Bool
    isPromotion (i,i')
      | take 3 (show i') == "120" = True
      | take 3 (show i') == "121" = True
      | take 3 (show i') == "122" = True
      | take 3 (show i') == "123" = True
      | otherwise                 = False
    {-
    - makes a promotion depending on what is the wanted promotion
    - 120 makes a Rook
    - 121 makes a Knight
    - 122 makes a Bishop
    - 123 makes a Queen
    -}
    makePromotion :: Board -> Move -> Board
    makePromotion (bs, c) (i,i')
      | take 3 (show i') == "120" = let i'' = read (drop 3 (show i')) in 
                                        move' (changeTurnColor (bs,c)) (i,i'') (Ps (Piece c Rook)) []
      | take 3 (show i') == "121" = let i'' = read (drop 3 (show i')) in 
                                        move' (changeTurnColor (bs,c)) (i,i'') (Ps (Piece c Knight)) []
      | take 3 (show i') == "122" = let i'' = read (drop 3 (show i')) in 
                                        move' (changeTurnColor (bs,c)) (i,i'') (Ps (Piece c Bishop)) []
      | take 3 (show i') == "123" = let i'' = read (drop 3 (show i')) in 
                                        move' (changeTurnColor (bs,c)) (i,i'') (Ps (Piece c Queen)) []
      | otherwise                 = ([(0,SquareError)],c)
