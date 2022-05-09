{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module HelperFunctions where 
import BoardAndPieces
import Debug.Trace 
import Debug.Trace 
import Data.Char 


----------------------------------------------------------
-- INTERFACE
----------------------------------------------------------


{-  squareToIndex s
    Returns an index of the square given.
    PRE: string cannot be empty
    RETURNS: index of square given as string
    EXAMPLES: squareToIndex "a8" == 21
-}
squareToIndex :: String -> Int

{-  indexToSquare i
    Returns the square of the index given.
    PRE: index must be between 21 and 98 including
    RETURNS: square of index given as Int 
    EXAMPLES: indexToSquare 21 == "a8"
-}
indexToSquare :: Int -> String 

{-  countMaterial board color
    Returns how many "centipawns" worth of material a side has.
    PRE: True
    RETURNS: amount of material on board for given color.
    EXAMPLES: countMaterial (fromFEN "8/8/4r3/8/8/8/3PP3/8 w") White == 200
              countMaterial (fromFEN "8/8/4r3/8/8/8/3PP3/8 w") Black == 500

    SIDENOTE: 
    The queen is worth 900
    Each rook is worth 500; 
    Each knight is worth 300; 
    Each bishop is worth 300; 
    Each pawn is worth 100 centipawns.
-}
countMaterial :: Board -> Color -> Int 

{-  boardLookupSquare board index 
    Returns the square at given index.
    PRE: board is not empty
    RETURNS: square at given index of board 
    EXAMPLES:   boardLookup ([(0,Out),(1,Out),(2,Out),(3,Out),(4,Out),(5,Out),(6,Out),
                            (7,Out),(8,Out),(9,Out),(10,Out),(11,Out),(12,Out),(13,Out),
                            (14,Out),(15,Out),(16,Out),(17,Out),(18,Out),(19,Out),(20,Out),
                            (21,Ps {p = Piece {c = Black, t = Rook}}),
                            (22,Ps {p = Piece {c = Black, t = Knight}}),
                            (23,Ps {p = Piece {c = Black, t = Bishop}}),
                            (24,Ps {p = Piece {c = Black, t = King}}),
                            (25,Ps {p = Piece {c = Black, t = Queen}}),
                            (26,Ps {p = Piece {c = Black, t = Bishop}}),
                            (27,Ps {p = Piece {c = Black, t = Knight}}),
                            (28,Ps {p = Piece {c = Black, t = Rook}}),
                            (29,Out),(30,Out),(31,Ps {p = Piece {c = Black, t = Pawn}}),
                            (32,Ps {p = Piece {c = Black, t = Pawn}}),
                            (33,Ps {p = Piece {c = Black, t = Pawn}}),
                            (34,Ps {p = Piece {c = Black, t = Pawn}}),
                            (35,Ps {p = Piece {c = Black, t = Pawn}}),
                            (36,Ps {p = Piece {c = Black, t = Pawn}}),
                            (37,Ps {p = Piece {c = Black, t = Pawn}}),
                            (38,Ps {p = Piece {c = Black, t = Pawn}}),(39,Out),
                            (40,Out),(41,Empty),(42,Empty),(43,Empty),(44,Empty),
                            (45,Empty),(46,Empty),(47,Empty),(48,Empty),(49,Out),
                            (50,Out),(51,Empty),(52,Empty),(53,Empty),(54,Empty),
                            (55,Empty),(56,Empty),(57,Empty),(58,Empty),(59,Out),
                            (60,Out),(61,Empty),(62,Empty),(63,Empty),(64,Empty),
                            (65,Empty),(66,Empty),(67,Empty),(68,Empty),(69,Out),
                            (70,Out),(71,Empty),(72,Empty),(73,Empty),(74,Empty),
                            (75,Empty),(76,Empty),(77,Empty),(78,Empty),(79,Out),
                            (80,Out),(81,Ps {p = Piece {c = White, t = Pawn}}),
                            (82,Ps {p = Piece {c = White, t = Pawn}}),
                            (83,Ps {p = Piece {c = White, t = Pawn}}),
                            (84,Ps {p = Piece {c = White, t = Pawn}}),
                            (85,Ps {p = Piece {c = White, t = Pawn}}),
                            (86,Ps {p = Piece {c = White, t = Pawn}}),
                            (87,Ps {p = Piece {c = White, t = Pawn}}),
                            (88,Ps {p = Piece {c = White, t = Pawn}}),
                            (89,Out),(90,Out),(91,Ps {p = Piece {c = White, t = Rook}}),
                            (92,Ps {p = Piece {c = White, t = Knight}}),
                            (93,Ps {p = Piece {c = White, t = Bishop}}),
                            (94,Ps {p = Piece {c = White, t = King}}),
                            (95,Ps {p = Piece {c = White, t = Queen}}),
                            (96,Ps {p = Piece {c = White, t = Bishop}}),
                            (97,Ps {p = Piece {c = White, t = Knight}}),
                            (98,Ps {p = Piece {c = White, t = Rook}}),(99,Out),
                            (100,Out),(101,Out),(102,Out),(103,Out),(104,Out),(105,Out),
                            (106,Out),(107,Out),(108,Out),(109,Out),(110,Out),(111,Out),
                            (112,Out),(113,Out),(114,Out),(115,Out),(116,Out),(117,Out),
                            (118,Out),(119,Out)],White)  0 == Out
                boardLookupSquare board 21 == Ps {p = Piece {c = Black, t = Rook}}
-}
boardLookupSquare :: Board -> Int -> Square 

{-  isMove str 
    Takes a string and decides whether it represents a move.
    EXAMPLES: "e2e4" == True 
              "post" == False
              "accepted done" == False
              "h8a1" == True 
-}
isMove :: String -> Bool 

------------------------------
--- IMPLEMENTATION
------------------------------

squareToIndex s
    | s == "a8" = 21
    | s == "b8" = 22
    | s == "c8" = 23
    | s == "d8" = 24
    | s == "e8" = 25
    | s == "f8" = 26
    | s == "g8" = 27
    | s == "h8" = 28
    | s == "a7" = 31
    | s == "b7" = 32
    | s == "c7" = 33
    | s == "d7" = 34
    | s == "e7" = 35
    | s == "f7" = 36
    | s == "g7" = 37
    | s == "h7" = 38
    | s == "a6" = 41
    | s == "b6" = 42
    | s == "c6" = 43
    | s == "d6" = 44
    | s == "e6" = 45
    | s == "f6" = 46
    | s == "g6" = 47
    | s == "h6" = 48 
    | s == "a5" = 51
    | s == "b5" = 52
    | s == "c5" = 53
    | s == "d5" = 54
    | s == "e5" = 55
    | s == "f5" = 56
    | s == "g5" = 57
    | s == "h5" = 58
    | s == "a4" = 61
    | s == "b4" = 62
    | s == "c4" = 63
    | s == "d4" = 64
    | s == "e4" = 65
    | s == "f4" = 66
    | s == "g4" = 67
    | s == "h4" = 68
    | s == "a3" = 71
    | s == "b3" = 72
    | s == "c3" = 73
    | s == "d3" = 74
    | s == "e3" = 75
    | s == "f3" = 76
    | s == "g3" = 77
    | s == "h3" = 78
    | s == "a2" = 81
    | s == "b2" = 82 
    | s == "c2" = 83
    | s == "d2" = 84
    | s == "e2" = 85
    | s == "f2" = 86
    | s == "g2" = 87
    | s == "h2" = 88
    | s == "a1" = 91
    | s == "b1" = 92
    | s == "c1" = 93
    | s == "d1" = 94
    | s == "e1" = 95
    | s == "f1" = 96
    | s == "g1" = 97
    | s == "h1" = 98 
    | otherwise = error "Insert a valid square, please."



indexToSquare i
    | i == 21 = "a8"
    | i == 22 = "b8"
    | i == 23 = "c8"
    | i == 24 = "d8" 
    | i == 25 = "e8" 
    | i == 26 = "f8"
    | i == 27 = "g8"
    | i == 28 = "h8"
    | i == 31 = "a7"
    | i == 32 = "b7"
    | i == 33 = "c7"
    | i == 34 = "d7"
    | i == 35 = "e7"
    | i == 36 = "f7"
    | i == 37 = "g7"
    | i == 38 = "h7"
    | i == 41 = "a6"
    | i == 42 = "b6"
    | i == 43 = "c6"
    | i == 44 = "d6"
    | i == 45 = "e6"
    | i == 46 = "f6"
    | i == 47 = "g6"
    | i == 48 = "h6" 
    | i == 51 = "a5" 
    | i == 52 = "b5"
    | i == 53 = "c5"
    | i == 54 = "d5"
    | i == 55 = "e5"
    | i == 56 = "f5"
    | i == 57 = "g5"
    | i == 58 = "h5"
    | i == 61 = "a4"
    | i == 62 = "b4"
    | i == 63 = "c4"
    | i == 64 = "d4"
    | i == 65 = "e4"
    | i == 66 = "f4"
    | i == 67 = "g4"
    | i == 68 = "h4"
    | i == 71 = "a3"
    | i == 72 = "b3"
    | i == 73 = "c3"
    | i == 74 = "d3"
    | i == 75 = "e3"
    | i == 76 = "f3"
    | i == 77 = "g3"
    | i == 78 = "h3"
    | i == 81 = "a2"
    | i == 82 = "b2" 
    | i == 83 = "c2"
    | i == 84 = "d2"
    | i == 85 = "e2"
    | i == 86 = "f2"
    | i == 87 = "g2"
    | i == 88 = "h2"
    | i == 91 = "a1"
    | i == 92 = "b1"
    | i == 93 = "c1"
    | i == 94 = "d1"
    | i == 95 = "e1"
    | i == 96 = "f1"
    | i == 97 = "g1"
    | i == 98 = "h1" 
    | otherwise = error "Insert a valid index, please."

countMaterial board color = countPawns pieces + countBishops pieces + countKnights pieces 
    + countRooks pieces + countQueens pieces + countKing pieces 
            where pieces = listPiecesColor (listPieces (listSquares (fst board))) color 
                  
                  countPawns pieces   = length (filter (\x -> t x == Pawn) pieces) * 100
                  countBishops pieces = length (filter (\x -> t x == Bishop) pieces) * 300
                  countKnights pieces = length (filter (\x -> t x == Knight) pieces) * 300
                  countRooks pieces   = length (filter (\x -> t x == Rook) pieces) * 500
                  countQueens pieces  = length (filter (\x -> t x == Queen) pieces) * 900
                  countKing pieces    = length (filter (\x -> t x == King) pieces) * 10000

                  listSquares []     = [] 
                  listSquares (x:xs) = if snd x /= Out && snd x /= Empty then [snd x] ++ listSquares xs else listSquares xs 

                  listPieces []     = []
                  listPieces (x:xs) = [p x] ++ listPieces xs

                  listPiecesColor pieces color = filter (\x -> c x == color) pieces 

{- SIDE NOTES: blackKnight = Piece Black Knight
    c blackKnight == Black 
    t blackKnight == Knight 

    square = Ps blackKnight
    p square == Piece {c = Black, t = Knight}
-}

boardLookupSquare ([], _) _  = error "Invalid square"
boardLookupSquare (((a, b):c:d), e) i
    | i == a = b
    | i == fst c = snd c
    | otherwise = boardLookupSquare (d,e) i
boardLookupSquare ([(a,b)], c) i 
     | i == a = b 
     | otherwise = error "Please insert a valid index."


isMove [] = False
isMove [_] = False
isMove [_,_] = False 
isMove [_,_,_] = False  
isMove (string@(x:y:w:z:p))
        | length (string) == 4 = if (x `elem` ['a' .. 'h'] && w `elem` ['a' .. 'h']) && 
            (y `elem` ['1' .. '8'] && z `elem` ['1' .. '8']) then True else False 
        | otherwise = if (x `elem` ['a' .. 'h'] && w `elem` ['a' .. 'h']) && 
            (y `elem` ['1' .. '8'] && z `elem` ['1' .. '8']) && (head p) `elem` ['Q', 'q', 'N', 'n', 'R', 'r', 'B', 'b']
                    then True else False 
