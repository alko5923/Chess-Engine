import Test.HUnit
import Engine
import HelperFunctions
import BoardAndPieces

--------------------------------------
-- Basic Engine Functionality
-- USAGE: Load this module in GHCI and execute
--          >runtests
-- -----------------------------------

mateScholars = fromFEN "rnbqk2r/pppp1Qpp/5n2/2b1p3/2B1P3/8/PPPP1PPP/RNB1K1NR b"
noMateScholars = fromFEN "rnb1k1nr/pppp1Qpp/8/2b1p3/2B1P2q/8/PPPP1PPP/RNB1K1NR b"


-- Conversion FEN-string/Board conversion 
test1 = TestCase $ assertEqual "Black pieces FEN" ([(0,Out),(1,Out),(2,Out),(3,Out),
    (4,Out),(5,Out),(6,Out),(7,Out),(8,Out),(9,Out),(10,Out),(11,Out),(12,Out),(13,Out),
    (14,Out),(15,Out),(16,Out),(17,Out),(18,Out),(19,Out),(20,Out),
    (21,Ps {p = Piece {c = Black, t = Rook}}),(22,Ps {p = Piece {c = Black, t = Knight}}),
    (23,Ps {p = Piece {c = Black, t = Bishop}}),(24,Ps {p = Piece {c = Black, t = Queen}}),
    (25,Out),(26,Out),(27,Empty),(28,Ps {p = Piece {c = Black, t = Pawn}}),
    (29,Ps {p = Piece {c = Black, t = Pawn}}),(30,Empty),(31,Out),(32,Out),(33,Empty),
    (34,Empty),(35,Empty),(36,Empty),(37,Out),(38,Out),(39,Out),(40,Out),(41,Out),(42,Out),
    (43,Out),(44,Out),(45,Out),(46,Out),(47,Out),(48,Out),(49,Out),(50,Out),(51,Out),(52,Out),
    (53,Out),(54,Out),(55,Out),(56,Out),(57,Out)],White)
 (fromFEN "rnbq/1pp1/4 w") 
test2 = TestCase $ assertEqual "Board fromFEN after move e4" ([(0,Out),(1,Out),(2,Out),
    (3,Out),(4,Out),(5,Out),(6,Out),(7,Out),(8,Out),(9,Out),(10,Out),(11,Out),(12,Out),
    (13,Out),(14,Out),(15,Out),(16,Out),(17,Out),(18,Out),(19,Out),(20,Out),
    (21,Ps {p = Piece {c = Black, t = Rook}}),(22,Ps {p = Piece {c = Black, t = Knight}}),
    (23,Ps {p = Piece {c = Black, t = Bishop}}),(24,Ps {p = Piece {c = Black, t = Queen}}),
    (25,Ps {p = Piece {c = Black, t = King}}),(26,Ps {p = Piece {c = Black, t = Bishop}}),
    (27,Ps {p = Piece {c = Black, t = Knight}}),(28,Ps {p = Piece {c = Black, t = Rook}}),
    (29,Out),(30,Out),(31,Ps {p = Piece {c = Black, t = Pawn}}),(32,Ps {p = Piece {c = Black, t = Pawn}}),
    (33,Ps {p = Piece {c = Black, t = Pawn}}),(34,Ps {p = Piece {c = Black, t = Pawn}}),
    (35,Ps {p = Piece {c = Black, t = Pawn}}),(36,Ps {p = Piece {c = Black, t = Pawn}}),
    (37,Ps {p = Piece {c = Black, t = Pawn}}),(38,Ps {p = Piece {c = Black, t = Pawn}}),
    (39,Out),(40,Out),(41,Empty),(42,Empty),(43,Empty),(44,Empty),(45,Empty),(46,Empty),
    (47,Empty),(48,Empty),(49,Out),(50,Out),(51,Empty),(52,Empty),(53,Empty),(54,Empty),
    (55,Empty),(56,Empty),(57,Empty),(58,Empty),(59,Out),(60,Out),(61,Empty),(62,Empty),
    (63,Empty),(64,Empty),(65,Ps {p = Piece {c = White, t = Pawn}}),(66,Empty),(67,Empty),
    (68,Empty),(69,Out),(70,Out),(71,Empty),(72,Empty),(73,Empty),(74,Empty),(75,Empty),
    (76,Empty),(77,Empty),(78,Empty),(79,Out),(80,Out),(81,Ps {p = Piece {c = White, t = Pawn}}),
    (82,Ps {p = Piece {c = White, t = Pawn}}),(83,Ps {p = Piece {c = White, t = Pawn}}),
    (84,Ps {p = Piece {c = White, t = Pawn}}),(85,Empty),(86,Ps {p = Piece {c = White, t = Pawn}}),
    (87,Ps {p = Piece {c = White, t = Pawn}}),(88,Ps {p = Piece {c = White, t = Pawn}}),
    (89,Out),(90,Out),(91,Ps {p = Piece {c = White, t = Rook}}),(92,Ps {p = Piece {c = White, t = Knight}}),
    (93,Ps {p = Piece {c = White, t = Bishop}}),(94,Ps {p = Piece {c = White, t = Queen}}),
    (95,Ps {p = Piece {c = White, t = King}}),(96,Ps {p = Piece {c = White, t = Bishop}}),
    (97,Ps {p = Piece {c = White, t = Knight}}),(98,Ps {p = Piece {c = White, t = Rook}}),
    (99,Out),(100,Out),(101,Out),(102,Out),(103,Out),(104,Out),(105,Out),(106,Out),(107,Out),
    (108,Out),(109,Out),(110,Out),(111,Out),(112,Out),(113,Out),(114,Out),(115,Out),(116,Out),
    (117,Out),(118,Out),(119,Out)],Black) (fromFEN "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b")
test3 = TestCase $ assertEqual "squareToIndex a8" 21 (squareToIndex "a8")
test4 = TestCase $ assertEqual "squareToIndex a8" 65 (squareToIndex "e4")
test5 = TestCase $ assertEqual "takeSquare board 23" (Ps {p = Piece {c = Black, t = Bishop}}) 
    (takeSquare (createStartBoard) 23) 
test6 = TestCase $ assertEqual "takeSquare board 21" (Ps {p = Piece {c = Black, t = Rook}}) 
    (takeSquare (createStartBoard) 21)
test7 = TestCase $ assertEqual "takeSquare board 31" (Out) 
    (takeSquare (createStartBoard) 40)

test8 = TestCase $ assertEqual "Count material starting position" 13900 (countMaterial createStartBoard White)
        
test9 = TestCase $ assertEqual "number of nodes in initial position" (20) 
             (length $ generateMoves (createStartBoard) []) 

test10 = TestCase $ assertEqual "number of nodes in position 2" 
            (48) (length $ generateMoves (fromFEN "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w") [])

test11 = TestCase $ assertEqual "number of nodes in position 3" 
            (14) (length $ generateMoves (fromFEN "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w") [])

test12 = TestCase $ assertEqual "number of nodes in position 4" 
            (6) (length $ generateMoves (fromFEN "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w") [])

test13 = TestCase $ assertEqual "number of nodes in position 5" 
            (41) (length $ generateMoves (fromFEN "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w") [])

test14 = TestCase $ assertEqual "number of nodes in position 6" 
            (46) (length $ generateMoves (fromFEN "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w") [])

test15 = TestCase $ assertEqual "isMove 'e2e4'" True (isMove "e2e4")

test16 = TestCase $ assertEqual "isMove 'h7h8Q'" True (isMove "h7h8Q")

test17 = TestCase $ assertEqual "isMove 'ab32'" False (isMove "ab32")

test18 = TestCase $ assertEqual "isCheckmate scholars" True (isCheckmate mateScholars [])

test19 = TestCase $ assertEqual "isCheckmate scholars escape" False (isCheckmate noMateScholars [])

runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10,
                                 test11, test12, test13, test14, test15, test16, test17, test18, test19]


