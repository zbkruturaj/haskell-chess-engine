module Evaluator where

import Board
import Data.Bits


---- Evaluation Plan ------

---- The following evaluation strategy is collected from around the internet and produced from observation. 

--	+ = for white
--	- = for black
--	assign weights to the pieces
--	bonus for king safety
--	bonus for having the bishop pair
--	increase value of bishops and rooks when the number of pawns decrease - i.e. more open games 
--	decrease value of knights when the number of pawns decrease - knights are not good in open games.
--	decrease value if a pawn is a double pawn - usually not good.
--	penalty for moving the queen too early in the game - do not want it to be chased around by the opponent
--	bonus for mobility - i.e. if a piece have more squares to move to it makes it more valuable.
--	bonus for threats - i.e. if a piece is threatening to capture an enemy piece it gets a bonus.
--	bonus for protecting own pieces - i.e. if it covers other pieces of the same colour it get a defensive bonus.
--	different activity bonus based on whether it's the opening, middle-game or endgame. For instance a king should not be active in the opening, but should be active in an 
--	penalty if there is many pawns of the same colour as a bishop on the same colour squares - i.e. a bad bishop vs a good bishop
--	bonus for pawns far advanced - i.e. close to promotion to a queen or other piece.
--	bonus for development speed of minor pieces in opening game.
--	In general chess divided into 3 parts-
--1.opening game
--2.middle game
--3.end game

--		1.opening game
--Primary objectives are:
--1.rapid development of own minor pieces and avoiding development of opponent's pieces
--2.control of centre(usually with pawns-generally achieved even if clear distinction in development of minor pieces)
--3.usually major pieces should never be moved in the opening part(rook,queen,king)
--4.king should be placed in a safe position(usually by castling but depends on the board position ex.if the position is too closed in the centre castling is not needed)
--5.generally material loss should be avoided but 1 or 2 pawns may be sacrificed if a very strong development is possible with opponent being quite undeveloped(ex.in queen's gambit).
--6.exchanges that lead to automatic development of pieces should be accepted
 
--->we can measure development of pieces by number of possible squares it can move-crude measure
--modified measure-a piece is developed if number of squares it can access+importance of those squares in the game position(relative to other pieces)
--a "developed piece" in one position may not be a "developed piece" in 2nd position even though it occupies same square


--		2.middle game
--primary objectives:
--1.safety of own king(adequate defence in response to possible attack)
--	king being kept out from the reach of opponent pieces(generally by castling)
--	placement of pieces to defend possible future attacks
--	king should not be open to threats so the pawn structure near the king should be neat (no double pawn,pawn near columns of king missing or is far too advanced to defend)
--2.attacking opponent king
--	opponent king should be made defenceless(by destroying its pawn structure,effectively removing major/minor pieces from its defence)
--	if a sacrifice is needed to checkmate the opponent king for sure, it should be done without any further thinking
--3.general position preferences
--	if game is closed i.e. high number of pieces present on the board, then minor pieces should not be exchanged with equivalent number of pawns+major pieces(ex.bishop with 3 pawns is not suggested)
--		unless some other significant positional advantage is obtained(like the 3 [awns were castle pawns of opp king)
--		and vice versa is also true
--	generally exchange of bishop with knight should be avoided unless it is a bad bishop or knight is placed at very good square
--	generally knight should be placed as close to the centre as possible since it gives him the most possible flexibility and threatens opponent in many ways
--		other pieces (bishop,rook,queen) needn't be placed near the centre but shouldn't be too much away from "happening position"(don't know how to explain the term)
--	pieces being trapped/pinned by less important pieces should be avoided
--	if your opponent has castled in different side,use your pawn structure to destroy his as soon as possible.In such case,it is suggested that you should attack opponent as quickly as possible
--	if you are getting attacked from one side,defend that side and if a more dangerous attack is possible on the other side do it
--	generally if u are attacked from one side,attack opponent from the centre since capturing the centre can help you defend the attack easily and also helps in counter attack.

		
--		3.end game

--	pawn structure matters a lot(pawns should be connected to each other,no double pawn etc.)
--	knight should be exchanged with bishop but check the position
--	pawns should be placed in same colour of your bishop(if you ve only one) and if only opponent has bishop your pawns should be placed in opposite colour
--	king should be as close to the centre to the possible
--	number of pieces matters more than quality of pieces present
--	your pawns should be as forward as possible try to avoid this for ur opponent
--	don't exchange passer pawn for a non-passer pawn
--	try to have a much higher ratio of pawns on one side and just less ration on the other side(ex.if both sides have 5 pawns,try to have 2 on 1 side and 3 on other and for opponent-4 on ur 3 side and 1 on ur 2 side)
--	if opponent king is nearby to ur king,place ur king so that after your move the opponent king is just at a gap of 1 square(ex e3,e5;e3,g5 etc.)
	 

--------Evaluation Function Coefficients--------------
--In order: Material Strength, Central Squares, Diagonal Analysis, Connected Pawns, Checkmate, Mobility.
coefficients = [10, 1, 1, 1, 10, 1]::[Int]

----------Material Strength----------------
mSC = [1000000, 9, 5, 3, 3, 1]
materialStrength :: State -> Int
materialStrength state = ((popCount (wk state))*(mSC!!0) + (popCount (wq state))*(mSC!!1) + (popCount (wr state))*(mSC!!2) + (popCount (wn state))*(mSC!!3) + (popCount (wb state))*(mSC!!4) + (popCount (wp state))*(mSC!!5)) - ((popCount (bk state))*(mSC!!0) + (popCount (bq state))*(mSC!!1) + (popCount (br state))*(mSC!!2) + (popCount (bb state))*(mSC!!3) + (popCount (bn state))*(mSC!!4) + (popCount (bp state))*(mSC!!5))

---------Connected Pawn Analysis-----------
--connectedPawns :: State -> Int

---
---



eval :: State -> Int
eval s = sum (zipWith (*) coefficients (map ($s) [materialStrength]))