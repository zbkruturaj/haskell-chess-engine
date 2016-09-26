module Board
	 where

-- Library Imports

-- |For Word64 dataType
import qualified Data.Word as IW  				
-- |For Bitwise operations
import Data.Bits as IB	
-- |For Masks
import qualified Data.Array.Unboxed as IU
-- |For String Arrays
import qualified Data.Array as IA
-- |For rfToP
import qualified Data.Char as IC
-- |For List Operations in nextState 
import qualified Data.List as IL
-- Project Imports

-- DataTypes
type BB = IW.Word64 -- For BitBoards

data Color = Black | White deriving (Eq,Show)

data Hist = Hist {enPassentPos :: Int
				, bQueenSideCastling :: Bool
				, bKingSideCastling :: Bool
				, wQueenSideCastling :: Bool
				, wKingSideCastling :: Bool
				}

type MoveList = [String]

instance Show Hist where
 show (Hist a  b c d e) = show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++ show e
 
-- enPassentPos gives file where 2-step pawn move has just been done. 8 if enpassent is not possible
-- There are a number of cases when castling is not permitted.
--Your king has been moved earlier in the game.
--The rook that castles has been moved earlier in the game.
--There are pieces standing between your king and rook.
--The king is in check.
--The king moves through a square that is attacked by a piece of the opponent.
--The king would be in check after castling. 


data State = State { history :: Hist
  					     , wp :: IW.Word64, wn :: IW.Word64, wb :: IW.Word64, wr :: IW.Word64, wq :: IW.Word64, wk :: IW.Word64
                         , bp :: IW.Word64, bn :: IW.Word64, bb :: IW.Word64, br :: IW.Word64, bq :: IW.Word64, bk :: IW.Word64
                     	 }
instance Show State where 
	show (State a b c d e f g h i j k l m) = show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++ show e ++ show f ++ "," ++ show g ++ "," ++ show h ++ "," ++ show i ++ "," ++ show j ++ show k ++ "," ++ show l ++ "," ++ show m
-- wp = White Pawns, wn = White Knights, wb = White Bishop, wr = White Rook, wq = White Queen, wk = White King
-- bp = Black Pawns, bn = Black Knights, bb = Black Bishop, br = Black Rook, bq = Black Queen, bk = Black King
-- history records history as a set of moves for the sake of enPassent and Castling.

type Move = ([Char], State)

--posToRF :: String -> Array
posToRF = IA.listArray (0,63) ["a1","b1","c1","d1","e1","f1","g1","h1",
							   "a2","b2","c2","d2","e2","f2","g2","h2",
							   "a3","b3","c3","d3","e3","f3","g3","h3",
							   "a4","b4","c4","d4","e4","f4","g4","h4",
							   "a5","b5","c5","d5","e5","f5","g5","h5",
							   "a6","b6","c6","d6","e6","f6","g6","h6",
							   "a7","b7","c7","d7","e7","f7","g7","h7",
							   "a8","b8","c8","d8","e8","f8","g8","h8"]
-- (wp s), (wk s), (wq s), (wb s), (wn s), (wr s), (bp s), (bk s), (bq s), (bb s), (bn s), (br s)
pieces = IA.listArray (0,11) ["wp", "wk", "wq", "wb", "wn", "wr","bp", "bk", "bq", "bb", "bn", "br"]

pToRF :: Int -> String
pToRF i = (posToRF IA.! i)

rfToP :: String -> Int
rfToP str = (((IC.ord (str!!1))-49)*8 + (IC.ord (str!!0) -97))
-- Move Generation Begins Here --
-- All final move generation functions should take in a State and give out a moveString(UCI Format)
-- There will be a nextStates function that would take all the moveString and give out the nextStates.

--UCI format reference:
-- normal move c1d2 showing a move from c1 to d2
-- castling like the normal move format, take kings src and dest into account
-- promotion b1a1q means pawn moved from b1 to a1 and got promoted to queen

-- Rooks --
-- Masks are precalculated --
-- Rook Masks --

rookHMasks :: IU.UArray Int BB
rookHMasks = IU.listArray (0,63) [254,253,251,247,239,223,191,127,65024,64768,64256,63232,61184,57088,48896,
									  32512,16646144,16580608,16449536,16187392,15663104,14614528,12517376,8323072,
									  4261412864,4244635648,4211081216,4143972352,4009754624,3741319168,3204448256,
									  2130706432,1090921693184,1086626725888,1078036791296,1060856922112,1026497183744,
									  957777707008,820338753536,545460846592,279275953455104,278176441827328,
									  275977418571776,271579372060672,262783279038464,245191092994048,210006720905216,
									  139637976727552,71494644084506624,71213169107795968,70650219154374656,
									  69524319247532032,67272519433846784,62768919806476288,53761720551735296,
									  35747322042253312,18302628885633695744,18230571291595767808,18086456103519911936,
									  17798225727368200192,17221764975064776704,16068843470457929728,13763000461244235776,
									  9151314442816847872]


rookVMasks :: IU.UArray Int BB
rookVMasks = IU.listArray (0,63) [72340172838076672,144680345676153344,289360691352306688,578721382704613376,
									  1157442765409226752,2314885530818453504,4629771061636907008,9259542123273814016,
									  72340172838076417,144680345676152834,289360691352305668,578721382704611336,
									  1157442765409222672,2314885530818445344,4629771061636890688,9259542123273781376,
									  72340172838011137,144680345676022274,289360691352044548,578721382704089096,
									  1157442765408178192,2314885530816356384,4629771061632712768,9259542123265425536,
									  72340172821299457,144680345642598914,289360691285197828,578721382570395656,
									  1157442765140791312,2314885530281582624,4629771060563165248,9259542121126330496,
									  72340168543109377,144680337086218754,289360674172437508,578721348344875016,
									  1157442696689750032,2314885393379500064,4629770786759000128,9259541573518000256,
									  72339073326448897,144678146652897794,289356293305795588,578712586611591176,
									  1157425173223182352,2314850346446364704,4629700692892729408,9259401385785458816,
									  72058697861366017,144117395722732034,288234791445464068,576469582890928136,
									  1152939165781856272,2305878331563712544,4611756663127425088,9223513326254850176,
									  282578800148737,565157600297474,1130315200594948,2260630401189896,4521260802379792,
									  9042521604759584,18085043209519168,36170086419038336]

-- Rook Move Helpers --
			
genRookVMoves :: Int -> BB -> BB
genRookVMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - 2*(reverse' sli)))) .&. mask
						where
							sli = (createBB [pos])
							mask = (rookVMasks IU.! pos)
							
genRookHMoves :: Int -> BB -> BB
genRookHMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - 2*(reverse' sli)))) .&. mask
						where
							sli = (createBB [pos])
							mask = (rookHMasks IU.! pos)

-- Result Moves Functions --

genRookMoves :: Int -> BB -> BB	-> BB
genRookMoves pos occ self = ((genRookVMoves pos occ) .|. (genRookHMoves pos occ)) .&. (complement self)

genWRookMovesFromState :: State -> [(Int,BB)]
genWRookMovesFromState s = (map (\t -> (t,(genRookMoves t (occ s) (whites s)))) (fromBB (wr s)))

genBRookMovesFromState :: State -> [(Int,BB)]
genBRookMovesFromState s = (map (\t -> (t,(genRookMoves t (occ s) (blacks s)))) (fromBB (br s)))

genWRookMovesFinal :: State -> [String]
genWRookMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genWRookMovesFromState s))

genBRookMovesFinal :: State -> [String]
genBRookMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genBRookMovesFromState s))

-- Bishops --
-- Masks are precalculated --
-- Bishop Masks --
bishopFMasks :: IU.UArray Int BB
bishopFMasks = IU.listArray (0,63) [0,256,66048,16909312,4328785920,1108169199616,283691315109888,72624976668147712,2,
									65540,16908296,4328783888,1108169195552,283691315101760,72624976668131456,
									145249953336262656,516,16778248,4328523792,1108168675360,283691314061376,
									72624976666050688,145249953332101120,290499906664136704,132104,4295231504,
									1108102090784,283691180892224,72624976399712384,145249952799424512,
									290499905598783488,580999811180789760,33818640,1099579265056,283674135240768,
									72624942308409472,145249884616818688,290499769233571840,580999538450366464,
									1161999072605765632,8657571872,281492291854400,72620578621636736,
									145241157243273216,290482314486480896,580964628956184576,1161929253617401856,
									2323857407723175936,2216338399296,72062026714726528,144124053429452800,
									288248106858840064,576496213700902912,1152992423106838528,2305983746702049280,
									4611686018427387904,567382630219904,1134765260439552,2269530520813568,
									4539061024849920,9078117754732544,18155135997837312,36028797018963968,0]


bishopLMasks :: IU.UArray Int BB
bishopLMasks = IU.listArray (0,63) [9241421688590303744,36099303471055872,141012904183808,550831656960,2151686144,
									8404992,32768,0,4620710844295151616,9241421688590303233,36099303471054850,
									141012904181764,550831652872,2151677968,8388640,64,2310355422147510272,
									4620710844295020800,9241421688590041601,36099303470531586,141012903135236,
									550829559816,2147491856,16416,1155177711056977920,2310355422114021376,
									4620710844228043008,9241421688456086017,36099303202620418,141012367312900,
									549757915144,4202512,577588851233521664,1155177702483820544,2310355404967706624,
									4620710809935413504,9241421619870827009,36099166032102402,140738026276868,1075843080,
									288793326105133056,577586656505233408,1155173313027244032,2310346626054553600,
									4620693252109107456,9241386504218214913,36028934726878210,275415828484,
									144115188075855872,288231475663339520,576462955621646336,1152925911260069888,
									2305851822520205312,4611703645040410880,9223407290080821761,70506452091906,0,
									281474976710656,564049465049088,1128103225065472,2256206466908160,4512412933881856,
									9024825867763968,18049651735527937]

-- Rook Move Helpers --

genBishopFMoves :: Int -> BB -> BB
genBishopFMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - (reverse'(2*sli))))) .&. mask
						where
							sli = (createBB [pos])
							mask = (bishopFMasks IU.! pos)	
							
genBishopLMoves :: Int -> BB -> BB
genBishopLMoves pos occ =	(xor ((occ.&.mask) - 2*sli) (reverse' ((reverse' (occ.&.mask)) - (reverse'(2*sli))))) .&. mask
						where
							sli = (createBB [pos])
							mask = (bishopLMasks IU.! pos)		

-- Result Moves Functions --							
genBishopMoves :: Int -> BB -> BB -> BB	
genBishopMoves pos occ self = ((genBishopLMoves pos occ) .|. 	(genBishopFMoves pos occ))	.&. (complement self)

genWBishopMovesFromState :: State -> [(Int,BB)]
genWBishopMovesFromState s = (map (\t -> (t,(genBishopMoves t (occ s) (whites s)))) (fromBB (wb s)))

genBBishopMovesFromState :: State -> [(Int,BB)]
genBBishopMovesFromState s = (map (\t -> (t,(genBishopMoves t (occ s) (blacks s)))) (fromBB (bb s)))

genWBishopMovesFinal :: State -> [String]
genWBishopMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genWBishopMovesFromState s))

genBBishopMovesFinal :: State -> [String]
genBBishopMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genBBishopMovesFromState s))

-- Queen constructed entirely out of genBishopMoves and genRookMoves
genQueenMoves :: Int -> BB -> BB -> BB	
genQueenMoves pos occ self = ((genBishopLMoves pos occ) .|. 	(genBishopFMoves pos occ) .|. (genRookVMoves pos occ) .|. 	(genRookHMoves pos occ)) .&. (complement self)

genWQueenMovesFromState :: State -> [(Int,BB)]
genWQueenMovesFromState s = (map (\t -> (t,(genQueenMoves t (occ s) (whites s)))) (fromBB (wq s)))

genBQueenMovesFromState :: State -> [(Int,BB)]
genBQueenMovesFromState s = (map (\t -> (t,(genQueenMoves t (occ s) (blacks s)))) (fromBB (bq s)))

genWQueenMovesFinal :: State -> [String]
genWQueenMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genWQueenMovesFromState s))

genBQueenMovesFinal :: State -> [String]
genBQueenMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genBQueenMovesFromState s))


-- Knight moves --
genKnightMoves :: Int -> BB -> BB
genKnightMoves pos self = (createBB (filter (>(-1)) (filter (<64) [pos+x|x<-[6,10,15,17,(-6),(-10),(-15),(-17)]])) .&. (knightRange pos)) .&. (complement self)

genWKnightMovesFromState :: State -> [(Int,BB)]
genWKnightMovesFromState s = (map (\t -> (t,(genKnightMoves t (whites s)))) (fromBB (wn s)))

genBKnightMovesFromState :: State -> [(Int,BB)]
genBKnightMovesFromState s = (map (\t -> (t,(genKnightMoves t (blacks s)))) (fromBB (bn s)))

genWKnightMovesFinal :: State -> [String]
genWKnightMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genWKnightMovesFromState s))

genBKnightMovesFinal :: State -> [String]
genBKnightMovesFinal s = foldl (++) [] (map (\(a,b) -> (map ((pToRF a) ++) (map (pToRF) (fromBB b)))) (genBKnightMovesFromState s))
-- Helper Function for Knight Moves --
knightRange :: Int -> BB
knightRange pos = createBB [8*x+y|x<-[hll..hul],y<-[vll..vul]]
				where
					(posx, posy) = (linToRF pos)
					hll = max 0 (posx-2)
					hul = min 7 (posx+2)
					vll = max 0 (posy-2)
					vul = min 7 (posy+2)
-- Pawn --
-- Masks are precalculated for enpassent and like --
-- Pawn Masks --

-- Pawn Moves --
-- White Pawn Moves --
genWPawnLeftAttacks :: BB -> BB -> BB -> BB
genWPawnLeftAttacks pawns opp self = (((pawns .&. (complement (createBB [8*x|x<-[0..7]]))) `shiftL` 7) .&. (opp) .&. (complement self))

genWPawnLAString :: BB -> BB -> BB -> [String]
genWPawnLAString pawns opp self = map (\a -> (pToRF (a-7) ++ pToRF (a))) (fromBB (genWPawnLeftAttacks pawns opp self))

genWPawnRightAttacks :: BB -> BB -> BB -> BB
genWPawnRightAttacks pawns opp self = (((pawns .&. (complement (createBB [8*x+7|x<-[0..7]]))) `shiftL` 9) .&. (opp) .&. (complement self))

genWPawnRAString :: BB -> BB -> BB -> [String]
genWPawnRAString pawns opp self = map (\a -> (pToRF (a-9) ++ pToRF (a))) (fromBB (genWPawnRightAttacks pawns opp self))

genWDPawnPushes :: BB -> BB -> BB
genWDPawnPushes pawns occ = (((((pawns .&. (createBB [8..15])) `shiftL` 8) .&. (complement occ)) `shiftL` 8).&. (complement occ))

genWSPawnPushes :: BB -> BB -> BB
genWSPawnPushes pawns occ = ((pawns `shiftL` 8) .&. (complement occ))

genWPawnSPString :: BB -> BB -> [String]
genWPawnSPString pawns occ = map (\a -> (pToRF (a-8) ++ pToRF (a))) (fromBB (genWSPawnPushes pawns occ))

genWPawnDPString :: BB -> BB -> [String]
genWPawnDPString pawns occ = map (\a -> (pToRF (a-16) ++ pToRF (a))) (fromBB (genWDPawnPushes pawns occ))

genWPawnAttacks :: BB -> BB -> BB -> BB
genWPawnAttacks pawns opp self = (genWPawnLeftAttacks pawns opp self) .|. (genWPawnRightAttacks pawns opp self)

genWEnPassentMoves :: State -> BB -> BB
genWEnPassentMoves state self_pawn = if (p == 8) then 0 
								else if testBit self_pawn (32 + p + 1)  then (createBB [40+p])
								else if testBit self_pawn (32 + p - 1)  then (createBB [40+p]) else 0
									where p = (enPassentPos (history state))

genWPawnEPString :: State -> BB -> [String]
genWPawnEPString state self = map (\a -> (pToRF (if testBit self (32 + p + 1)  then (32+p+1) else (32+p-1)) ++ pToRF (a))) (fromBB (genWEnPassentMoves state self))
							where
								p = (enPassentPos (history state))

genWPawnMoves :: BB -> BB -> BB -> BB -> BB 
genWPawnMoves pawns opp self occ = (genWSPawnPushes pawns occ) .|. (genWDPawnPushes pawns occ) .|. (genWPawnAttacks pawns opp self)

genWPawnMovesFinal :: State -> [String]
genWPawnMovesFinal s = (genWPawnEPString s self) ++ (genWPawnRAString pawns opp self) ++ (genWPawnLAString pawns opp self) ++ (genWPawnDPString pawns occu) ++ (genWPawnSPString pawns occu)
					where
						self = (whites s)
						opp  = (blacks s)
						occu = (occ s)
						pawns = (wp s)

-- take care of Promotions
-- take care of UCI - BoardState Output

-- Black Pawn Moves --
genBPawnLeftAttacks :: BB -> BB -> BB -> BB
genBPawnLeftAttacks pawns opp self = (((pawns .&. (complement (createBB [8*x|x<-[0..7]]))) `shiftR` 7) .&. (opp) .&. (complement self))

genBPawnLAString :: BB -> BB -> BB -> [String]
genBPawnLAString pawns opp self = map (\a -> (pToRF (a+7) ++ pToRF (a))) (fromBB (genBPawnLeftAttacks pawns opp self))

genBPawnRightAttacks :: BB -> BB -> BB -> BB
genBPawnRightAttacks pawns opp self = (((pawns .&. (complement (createBB [8*x+7|x<-[0..7]]))) `shiftR` 9) .&. (opp) .&. (complement self))

genBPawnRAString :: BB -> BB -> BB -> [String]
genBPawnRAString pawns opp self = map (\a -> (pToRF (a+9) ++ pToRF (a))) (fromBB (genBPawnRightAttacks pawns opp self))

genBDPawnPushes :: BB -> BB -> BB
genBDPawnPushes pawns occ = (((((pawns .&. (createBB [8..15])) `shiftR` 8) .&. (complement occ)) `shiftR` 8).&. (complement occ))

genBSPawnPushes :: BB -> BB -> BB
genBSPawnPushes pawns occ = ((pawns `shiftR` 8) .&. (complement occ))

genBPawnSPString :: BB -> BB -> [String]
genBPawnSPString pawns occ = map (\a -> (pToRF (a+8) ++ pToRF (a))) (fromBB (genBSPawnPushes pawns occ))

genBPawnDPString :: BB -> BB -> [String]
genBPawnDPString pawns occ = map (\a -> (pToRF (a+16) ++ pToRF (a))) (fromBB (genBDPawnPushes pawns occ))

genBPawnAttacks :: BB -> BB -> BB -> BB
genBPawnAttacks pawns opp self = (genBPawnLeftAttacks pawns opp self) .|. (genBPawnRightAttacks pawns opp self)

genBEnPassentMoves :: State -> BB -> BB
genBEnPassentMoves state self_pawn = if (p == 8) then 0 
								else if testBit self_pawn (24 + p + 1)  then (createBB [16+p])
								else if testBit self_pawn (24 + p - 1)  then (createBB [16+p]) else 0
									where p = (enPassentPos (history state))

genBPawnEPString :: State -> BB -> [String]
genBPawnEPString state self = map (\a -> (pToRF (if (testBit self (24 + p + 1)) then (24+p+1) else (24+p-1)) ++ pToRF (a))) (fromBB (genBEnPassentMoves state self))
							where
								p = (enPassentPos (history state))

genBPawnMoves :: BB -> BB -> BB -> BB -> BB
genBPawnMoves pawns opp self occ = (genBDPawnPushes pawns occ) .|. (genBSPawnPushes pawns occ) .|. (genBPawnAttacks pawns opp self)

genBPawnMovesFinal :: State -> [String]
genBPawnMovesFinal s = (genBPawnEPString s self) ++ (genBPawnRAString pawns opp self) ++ (genBPawnLAString pawns opp self) ++ (genBPawnDPString pawns occu) ++ (genBPawnSPString pawns occu)
					where
						self = (blacks s)
						opp  = (whites s)
						occu = (occ s)
						pawns = (bp s)

-- take care of Promotions
-- take care of UCI - BoardState Output

-- King Moves --
-- White --

-- All Places Blacks can attack the king
isBKingSafe :: State -> Bool
isBKingSafe s = (popCount ((unsafeBBB s occu opp self) .&. (bk s))) == 0
			where
				occu = occ s
				opp  = whites s
				self = blacks s

isWKingSafe :: State -> Bool
isWKingSafe s = (popCount ((unsafeBBW s occu opp self) .&. (wk s))) == 0
			where
				occu = occ s
				opp  = blacks s
				self = whites s

unsafeBBW :: State -> BB -> BB -> BB -> BB
unsafeBBW s occ opp self = ((genBPawnMoves (bp s) opp self occ) .|. (genBEnPassentMoves s (bp s)) .|.
							(foldl (.|.) 0 (map (snd) (genBRookMovesFromState s))) .|.
							(foldl (.|.) 0 (map (snd) (genBBishopMovesFromState s))) .|. 
							(foldl (.|.) 0 (map (snd) (genBQueenMovesFromState s)))	.|. 
							(foldl (.|.) 0 (map (snd) (genBKnightMovesFromState s))) .|.
							((genBKingMovesFromState s)))

-- Black --

-- All Places Whites can attack the king
unsafeBBB :: State -> BB -> BB -> BB -> BB
unsafeBBB s occ opp self = ((genWPawnMoves (wp s) opp self occ) .|. (genWEnPassentMoves s (wp s)) .|.
							(foldl (.|.) 0 (map (snd) (genWRookMovesFromState s))) .|.
							(foldl (.|.) 0 (map (snd) (genWBishopMovesFromState s))) .|. 
							(foldl (.|.) 0 (map (snd) (genWQueenMovesFromState s)))	.|. 
							(foldl (.|.) 0 (map (snd) (genWKnightMovesFromState s))) .|.
							((genWKingMovesFromState s)))

-- Castling Moves --
bCastling :: State -> [String]
bCastling s = [blackQueenSideCastling s] ++ [blackKingSideCastling s]

wCastling :: State -> [String]
wCastling s = [whiteQueenSideCastling s] ++ [whiteKingSideCastling s]

blackClearForQCastling :: State -> Bool
blackClearForQCastling s = (testBit b 60) && (testBit b 59) && (testBit b 58) && (testBit k 59)  && (testBit k 58)  && (testBit k 57) 
						where
						b = (unsafeBBB s k (whites s) (blacks s)) 
						k = occ s

whiteClearForQCastling :: State -> Bool
whiteClearForQCastling s = (testBit b 4) && (testBit b 3) && (testBit b 2) && (testBit k 1) && (testBit k 3) && (testBit k 2)  
						where
						b = (unsafeBBW s k (whites s) (blacks s)) 
						k = occ s 

blackClearForKCastling :: State -> Bool
blackClearForKCastling s = (testBit b 60) && (testBit b 61) && (testBit b 62) && (testBit k 62) && (testBit k 61) 
						where
						b = (unsafeBBB s k (whites s) (blacks s))  
						k = occ s

whiteClearForKCastling :: State -> Bool
whiteClearForKCastling s = (testBit b 4) && (testBit b 5) && (testBit b 6) && (testBit k 5) && (testBit k 6)  
						where
						b = (unsafeBBW s k (whites s) (blacks s))  
						k = occ s

blackQueenSideCastling :: State -> String 
blackQueenSideCastling s = if ((bQueenSideCastling (history s)) && (blackClearForQCastling s)) == True then "e7c7" else ""
						--where
							--new_s = State { history = new_h, wp = (wp s), wn = (wn s), wb = (wb s), wr = (wr s), wq = (wq s), wk = (wk s), bp = (bp s), bn = (bn s), bb = (bb s), br = new_br, bq = (bq s), bk = (createBB [58])}
							--new_br = (setBit (clearBit (br s) 56) 59)
							--new_h = Hist {enPassentPos = 8, bQueenSideCastling = False, bKingSideCastling = False, wQueenSideCastling = (wQueenSideCastling (history s)), wKingSideCastling = (wKingSideCastling (history s))}

blackKingSideCastling :: State -> String 
blackKingSideCastling s = if ((bKingSideCastling (history s)) && (blackClearForKCastling s)) == True then "e7g7" else ""

whiteQueenSideCastling :: State -> String  
whiteQueenSideCastling s = if ((wQueenSideCastling (history s)) && (whiteClearForQCastling s)) == True then "e1c1" else ""

whiteKingSideCastling :: State -> String 
whiteKingSideCastling s = if ((wKingSideCastling (history s)) && (whiteClearForKCastling s)) == True then "e1g1" else ""
						
-- King Moves Helper Function --

genPrimitiveKingMoves :: Int -> BB -> BB
genPrimitiveKingMoves pos self = (createBB (filter (>(-1)) (filter (<64) [pos+x|x<-[1,7,8,9,(-1),(-7),(-8),(-9)]])) .&. (kingRange pos)) .&. (complement self)

genWKingMovesFromState :: State -> BB
genWKingMovesFromState s = (genPrimitiveKingMoves ((fromBB (wk s))!!0) (whites s))

genBKingMovesFromState :: State -> BB
genBKingMovesFromState s = (genPrimitiveKingMoves ((fromBB (bk s))!!0) (blacks s))

kingRange :: Int -> BB
kingRange pos = createBB [8*x+y|x<-[hll..hul],y<-[vll..vul]]
				where
					(posx, posy) = (linToRF pos)
					hll = max 0 (posx-1)
					hul = min 7 (posx+1)
					vll = max 0 (posy-1)
					vul = min 7 (posy+1)

genWKingMovesFinal :: State -> [String]
genWKingMovesFinal s = map func (map (pToRF) v)
					where
						func = ((pToRF (p !! 0)) ++)
						p = (fromBB (wk s))
						v = (fromBB (genPrimitiveKingMoves (p !! 0) (whites s)))

genBKingMovesFinal :: State -> [String]
genBKingMovesFinal s = map (pToRF (p !! 0) ++) (map (pToRF) v)
					where
						p = (fromBB (bk s))
						v = (fromBB (genPrimitiveKingMoves (p !! 0) (blacks s)))
---- Final Moves Function ----

genWMovesFinal :: State -> [String]
genWMovesFinal s = (genWRookMovesFinal s) ++ (genWKnightMovesFinal s) ++ (genWBishopMovesFinal s) ++ (genWQueenMovesFinal s) ++ (genWKingMovesFinal s) ++ (genWPawnMovesFinal s)

genBMovesFinal :: State -> [String]
genBMovesFinal s = (genBRookMovesFinal s) ++ (genBKnightMovesFinal s) ++ (genBBishopMovesFinal s) ++ (genBQueenMovesFinal s) ++ (genBKingMovesFinal s) ++ (genBPawnMovesFinal s)

---- State From Move String ----
nextStatesAdvanced :: (Color,State) -> [(Color,(String,State))]
nextStatesAdvanced (Black,s) = filter (isBKingSafe.snd.snd) (nextStates (Black,s))
nextStatesAdvanced (White,s) = filter (isWKingSafe.snd.snd) (nextStates (White,s))

nextStates :: (Color,State) -> [(Color,(String,State))]
nextStates (Black,state) = [(White,next_state)|next_state <- ((genNextStates state (genBMovesFinal state)))]
nextStates (White,state) = [(Black,next_state)|next_state <- ((genNextStates state (genWMovesFinal state)))]

genNextStates :: State -> [String] -> [(String,State)]
genNextStates state str_l = zip str_l (map (genNextState state) str_l)

genNextState :: State -> String -> State
genNextState state moveString 
	| (pieceAtRF state (take 2 moveString)) == "wk" = genNextStateForWKingMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "wp" = genNextStateForWPawnMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "wr" = genNextStateForWRookMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "wb" = genNextStateForWBishopMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "wq" = genNextStateForWQueenMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "wn" = genNextStateForWKnightMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "bk" = genNextStateForBKingMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "bp" = genNextStateForBPawnMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "br" = genNextStateForBRookMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "bb" = genNextStateForBBishopMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "bq" = genNextStateForBQueenMoves s moveString
	| (pieceAtRF state (take 2 moveString)) == "bn" = genNextStateForBKnightMoves s moveString
	| otherwise = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where s = (clearDest state moveString)

clearDest :: State -> String -> State
clearDest state moveString 
	| (testBit (wk state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  new_wk, bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (wp state) f_pos) = State {history = (history state), wp = new_wp, wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (wr state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = new_wr, wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (wb state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = new_wb, wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (wq state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = new_wk, wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (wn state) f_pos) = State {history = (history state), wp = (wp state), wn =  new_wn, wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (bk state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = new_bk}
	| (testBit (bp state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = new_bp, bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (br state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = new_br, bq = (bq state), bk = (bk state)}
	| (testBit (bb state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = new_bb, br = (br state), bq = new_bq, bk = (bk state)}
	| (testBit (bq state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (testBit (bn state) f_pos) = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = new_bn, bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (enPassentPos (history state) < 8) && ((testBit (wp state) (f_pos+8))) =  State {history = (history state), wp = epm_wp, wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (enPassentPos (history state) < 8) && ((testBit (bp state) (f_pos-8))) =  State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = epm_bp, bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| otherwise = State {history = (history state), wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where 
		new_wk = (clearBit (wk state) f_pos)
		new_wq = (clearBit (wq state) f_pos)
		new_wr = (clearBit (wr state) f_pos)
		new_wb = (clearBit (wb state) f_pos)
		new_wn = (clearBit (wn state) f_pos)
		new_wp = (clearBit (wp state) f_pos)
		new_bk = (clearBit (bk state) f_pos)
		new_bq = (clearBit (bq state) f_pos)
		new_br = (clearBit (br state) f_pos)
		new_bb = (clearBit (bb state) f_pos)
		new_bn = (clearBit (bn state) f_pos)
		new_bp = (clearBit (bp state) f_pos)
		epm_wp = (clearBit (wp state) (f_pos+8))
		epm_bp = (clearBit (bp state) (f_pos-8))
		f_pos = rfToP (drop 2 moveString)

genNextStateForWKingMoves :: State -> String -> State
genNextStateForWKingMoves state moveString
	| moveString == "e1g1" = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = new_wr_k, wq = (wq state), wk =  new_wk_k, bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}--KingSideCastling
	| moveString == "e1c1" = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = new_wr_q, wq = (wq state), wk =  new_wk_q, bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}--QueenSideCastling
	| otherwise = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  new_wk, bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_h = Hist {enPassentPos = 8, wQueenSideCastling = False, wKingSideCastling = False, bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_wk = (setBit (clearBit (wk state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)
		new_wk_k = (setBit (clearBit (wk state) 4) 6)
		new_wr_k = (setBit (clearBit (wr state) 7) 5)
		new_wk_q = (setBit (clearBit (wk state) 4) 2)
		new_wr_q = (setBit (clearBit (wr state) 0) 3)

wEPen = ["a2a4","b2b4","c2c4","d2d4","e2e4","f2f4","g2g4","h2h4"]
genNextStateForWPawnMoves :: State -> String -> State
genNextStateForWPawnMoves state moveString
	| elem moveString wEPen = State {history = new_h, wp = new_wp, wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| otherwise = State {history = new_hn, wp = new_wp, wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_h = Hist {enPassentPos = ((IL.elemIndices moveString wEPen)!!0), wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_wp = (setBit (clearBit (wp state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForWRookMoves :: State -> String -> State
genNextStateForWRookMoves state moveString
	| (take 2 moveString) == "a1" = State {history = Hist {enPassentPos = 8, wQueenSideCastling = False, wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}, wp = (wp state), wn =  (wn state), wb = (wb state), wr = new_wr, wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| (take 2 moveString) == "h1" = State {history = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = False, bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}, wp = (wp state), wn =  (wn state), wb = (wb state), wr = new_wr, wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| otherwise = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = new_wr, wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_wr = (setBit (clearBit (wr state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForWKnightMoves :: State -> String -> State
genNextStateForWKnightMoves state moveString = State {history = new_hn, wp = (wp state), wn =  new_wn, wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_wn = (setBit (clearBit (wn state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForWBishopMoves :: State -> String -> State
genNextStateForWBishopMoves state moveString = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = new_wb, wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_wb = (setBit (clearBit (wb state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForWQueenMoves :: State -> String -> State
genNextStateForWQueenMoves state moveString = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = new_wq, wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_wq = (setBit (clearBit (wq state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForBKingMoves :: State -> String -> State
genNextStateForBKingMoves state moveString
	| moveString == "e7g7" = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = new_br_k, bq = (bq state), bk = new_bk_k}--KingSideCastling
	| moveString == "e7c7" = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = new_br_q, bq = (bq state), bk = new_bk_q}--QueenSideCastling
	| otherwise = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk =  (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = new_bk}
	where
		new_h = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = False, bKingSideCastling = False}
		new_bk = (setBit (clearBit (bk state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)
		new_bk_k = (setBit (clearBit (bk state) 60) 62)
		new_br_k = (setBit (clearBit (br state) 63) 61)
		new_bk_q = (setBit (clearBit (bk state) 60) 58)
		new_br_q = (setBit (clearBit (br state) 56) 59)

bEPen = ["a7a5","b7b5","c7c5","d7d5","e7e5","f7f5","g7g5","h7h5"]
genNextStateForBPawnMoves :: State -> String -> State
genNextStateForBPawnMoves state moveString
	| elem moveString bEPen = State {history = new_h, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = new_bp, bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	| otherwise = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = new_bp, bn = (bn state), bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_h = Hist {enPassentPos = ((IL.elemIndices moveString bEPen)!!0), wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_bp = (setBit (clearBit (bp state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForBRookMoves :: State -> String -> State
genNextStateForBRookMoves state moveString
	| (take 2 moveString) == "a7" = State {history = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = False, bKingSideCastling = (bKingSideCastling (history state))}, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = new_br, bq = (bq state), bk = (bk state)}
	| (take 2 moveString) == "h7" = State {history = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = False}, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = new_br, bq = (bq state), bk = (bk state)}
	| otherwise = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = new_br, bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_br = (setBit (clearBit (br state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForBKnightMoves :: State -> String -> State
genNextStateForBKnightMoves state moveString = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = new_bn, bb = (bb state), br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_bn = (setBit (clearBit (bn state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForBBishopMoves :: State -> String -> State
genNextStateForBBishopMoves state moveString = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = new_bb, br = (br state), bq = (bq state), bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_bb = (setBit (clearBit (bb state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

genNextStateForBQueenMoves :: State -> String -> State
genNextStateForBQueenMoves state moveString = State {history = new_hn, wp = (wp state), wn =  (wn state), wb = (wb state), wr = (wr state), wq = (wq state), wk = (wk state), bp = (bp state), bn = (bn state), bb = (bb state), br = (br state), bq = new_bq, bk = (bk state)}
	where
		new_hn = Hist {enPassentPos = 8, wQueenSideCastling = (wQueenSideCastling (history state)), wKingSideCastling = (wKingSideCastling (history state)), bQueenSideCastling = (bQueenSideCastling (history state)), bKingSideCastling = (bKingSideCastling (history state))}
		new_bq = (setBit (clearBit (bq state) i_pos) f_pos)
		f_pos = rfToP (drop 2 moveString)
		i_pos = rfToP (take 2 moveString)

bbOfPieceAtRF :: State -> String -> BB
bbOfPieceAtRF s str = bbOfPieceAtPos s (rfToP str)

bbOfPieceAtPos :: State -> Int -> BB
bbOfPieceAtPos s pos = (filter ((flip testBit) pos) [(wp s), (wk s), (wq s), (wb s), (wn s), (wr s), (bp s), (bk s), (bq s), (bb s), (bn s), (br s)]) !! 0

pieceAtRF :: State -> String -> String
pieceAtRF s str = pieceAtPos s (rfToP str)

pieceAtPos :: State -> Int -> String
pieceAtPos s pos = pieces IA.! ((IL.findIndices ((flip testBit) pos) [(wp s), (wk s), (wq s), (wb s), (wn s), (wr s), (bp s), (bk s), (bq s), (bb s), (bn s), (br s)]) !! 0)

-- General Helper Functions -- 

print64 :: BB -> IO ()
print64 b = let l64 = replicate 64 b
                shifts = 1:(take 63 (map (shift 1) [1..])) :: [BB]
                inter = zipWith (.&.) shifts l64
                result = (map fromIntegral) $ zipWith shift inter (map (*(-1))[0..63]) :: [Int]
            in printLikeABoardIterate 0 result

printLikeABoardIterate :: Int -> [Int] -> IO ()
printLikeABoardIterate 64 _ = putStr "\n"
printLikeABoardIterate i xs = do let realIndex = \a -> 64 - 8*( truncate ((fromIntegral (a+8))/8) ) + (a `rem` 8) :: Int
                                 if i `rem` 8 == 0
                                     then putStr $ "\n"
                                     else return ()
                                 let x = (xs !! (realIndex i))
                                 putStr $ ( show x ) ++ " "
                                 printLikeABoardIterate (i+1) xs

reverse' :: BB -> BB
reverse' brd = createBB [(63-i)|i<-[0..63],(testBit brd i)] 

createBB :: [Int] -> BB
createBB xs = foldr (flip$setBit) 0 xs

fromBB :: BB -> [Int]
fromBB b = filter (testBit b) [0..63] 

linToRF :: Int -> (Int, Int)
linToRF sq = quotRem sq 8

whites :: State -> BB
whites s = (wp s) .|. (wn s) .|. (wb s) .|. (wr s) .|. (wq s) .|. (wk s)

blacks :: State -> BB
blacks s = (bp s) .|. (bn s) .|. (bb s) .|. (br s) .|. (bq s) .|. (bk s)

occ :: State -> BB
occ s = (bp s) .|. (bn s) .|. (bb s) .|. (br s) .|. (bq s) .|. (bk s) .|. (wp s) .|. (wn s) .|. (wb s) .|. (wr s) .|. (wq s) .|. (wk s)

-- Functions For Testing -- 
--o = createBB  ([0..15] ++ [48..63])
--s = createBB [0..15]
o = createBB[26]
h :: Hist
h = Hist {enPassentPos = 3, bQueenSideCastling = False, bKingSideCastling = False, wKingSideCastling = False, wQueenSideCastling = False}
s :: State
s = State {history = h, wp = 0, wn =  0, wb = 0, wr = 0, wq = 0, wk =  0, bp = 0, bn = 0, bb = 0, br = 0, bq = 0, bk = 0}

t_h = Hist {enPassentPos = 1, bQueenSideCastling = True, bKingSideCastling = True, wQueenSideCastling = True, wKingSideCastling = True}

t_s = State {history = t_h, wp = (createBB [8..15]), wn= (createBB [1,6]), wb = (createBB [2,5]), wr = (createBB [0,7]), wq = (createBB [3]), wk = (createBB [4]), bp= (createBB [48..55]), bn = (createBB [62,57]),bb = (createBB [58,61]), br = (createBB [56,63]), bq = (createBB [59]),bk = (createBB [60])}

t_attack = State {history = t_h, wp = (createBB [32]), wn= (createBB []), wb = (createBB []), wr = (createBB []), wq = (createBB []), wk = (createBB [0]), bp= (createBB [33]), bn = (createBB []),bb = (createBB []), br = (createBB []), bq = (createBB []),bk = (createBB [63])}