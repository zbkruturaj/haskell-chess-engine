
data Hist = Hist {enPassentPos :: Int
				, bQueenSideCastling :: Bool
				, bKingSideCastling :: Bool
				, wQueenSideCastling :: Bool
				, wKingSideCastling :: Bool
				}

instance Show Hist where
 show (Hist a b c d e) = show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++ show e

h = Hist {enPassentPos = 3, bQueenSideCastling = False, bKingSideCastling= False, wQueenSideCastling= False, wKingSideCastling= False}