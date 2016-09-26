module Search 
	where 

import Board
import Evaluator

data GameTree = GameTree {state::(Color,(String,State)), children::[GameTree]} deriving Show

alpha::Int
alpha = -40000

beta::Int
beta = 40000

evalState::(Color,(String,State))->Int
evalState (c,(m,s)) = eval s

genGameTree::Int->(Color,(String,State))->GameTree
genGameTree 0 s = GameTree s []
genGameTree maxdepth (c,(m,s)) = GameTree (c,(m,s)) (map (genGameTree (maxdepth-1)) (nextStatesAdvanced (c,s)))

alphabeta::GameTree->Int->Int->Int
alphabeta (GameTree s []) _ _ = evalState s
alphabeta (GameTree s [x]) a b = alphabeta x a b
alphabeta (GameTree (White, (m,s)) (x:xs)) a b
				| val >= b = val
				| otherwise = max val (alphabeta (GameTree (White, (m,s)) xs) a' b)
				where	val = (alphabeta x a b) `div` 2
					a' = max val a
alphabeta (GameTree (Black, (m,s)) (x:xs)) a b
				| val <= a = val
				| otherwise = min val (alphabeta (GameTree (Black, (m,s)) xs) a b')
				where	val = (alphabeta x a b) `div` 2
					b' = min val b

getNextState::(Color,(String,State))->(Color,(String,State))
getNextState curr = case (genGameTree depth curr) of
					GameTree p [] -> p
					GameTree (c, _) xs -> snd (findBestNextState c (compare c) (map (\x->(alphabeta x alpha beta, state x)) xs))
					where 
						compare White = (>)
						compare Black = (<)

getNextMove s = fst (snd (getNextState s))

findBestNextState::Color -> (Int -> Int -> Bool)  -> [(Int, (Color,(String,State)))] -> (Int, (Color,(String,State)))
findBestNextState _ _ [x] = x
findBestNextState f cmp ((x1,(c,(m,s))):xs) 
				| winningState (c,(m,s)) = (x1,(c,(m,s)))
				| otherwise = let (x2, y2) = findBestNextState f cmp xs in
                                             if cmp x1 x2 then (x1,(c,(m,s))) else (x2,y2)

winningState::(Color,(String,State))->Bool
winningState (c,(m,s)) = (null (nextStatesAdvanced (c,s)))
--getNextState  
--findBestNextState
--displayGame
depth :: Int
depth = 3