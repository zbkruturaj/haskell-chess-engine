--module UCI
--	(uci
--	) where 

--import Control.Applicative (liftA)
--import Data.IORef
--import System.Exit
--import System.IO
--import Evaluator
--import Board
--import Search
--import Text.ParserCombinators.Parsec

--data SearchOption = MovetimeMsc Int | Infinity deriving (Show)

--data Command = CmdUci
--			| CmdIsReady
--			| CmdUciNewGame
--			| CmdPosition MoveList
--			| CmdGo SearchOption
--			| CmdStop
--			| CmdQuit

--uciUciParser :: Parser Command
--uciUciParser = string "uci" >> return CmdUci

--uciIsReadyParser :: Parser Command
--uciIsReadyParser = string "isready" >> return CmdIsReady

--uciNewGameParser :: Parser Command
--uciNewGameParser = string "ucinewgame" >> return CmdUciNewGame

--uciStopParser :: Parser Command
--uciStopParser = string "stop" >> return CmdStop

--uciQuitParser :: Parser Command
--uciQuitParser = string "quit" >> return CmdQuit

--uciIntParser :: Parser Int
--uciIntParser = liftA read $ many1 digit

--uciGoParser :: Parser Command
--uciGoParser = do 
--				string "go" >> spaces
--                mbTimeout <- optionMaybe (string "movetime" >> spaces >> uciIntParser)
--                return $ case mbTimeout of
--                            Nothing -> CmdGo Infinity
--                            Just timeout -> CmdGo $ MovetimeMsc timeout

--uciPositionParser :: CharParser () Command
--uciPositionParser = do 
--	_ <- string "position" >> (many1 $ char ' ')
--	posType <- string "fen" <|> string "startpos"
--	spaces 
--	gameState <- return initialGameState
--	liftA CmdPosition (option )

module UCI where

import Data.IORef
import System.Exit
import Board
import Search
import Evaluator

process :: String -> String
process "uci" = "uci"
process "isready" = "isready"
process "ucinewgame" = "ucinewgame"
process s
	| (take 23 s) == "position startpos moves" = "pos"
	| (take 2 s) == "go" = "go"

uci :: IO ()
uci = do
    --hSetBuffering stdout NoBuffering
--    lastGameState <- newIORef initialGameState
    game <- newIORef (White,("",t_s))

    let dialogue = do
                line <- getLine
                case process line of
                    "uci" -> putStrLn "id name newHC\nid author Ruturaj\nuciok"
                    "isready" -> putStrLn "readyok"
                    "ucinewgame" -> do
                    					modifyIORef game (const (White,("",t_s)))
                    "pos" -> do
                    					tmp <- readIORef game
                    					modifyIORef game (const (Black,(((words (drop 5 line))!!0),(genNextState (snd(snd(tmp))) ((words (drop 5 line))!!0)))))
                    "go" -> do
                    					putStrLn "in go"
                    					tmp <- readIORef game
                    					putStrLn (show tmp)
                    					let mv = (getNextMove tmp)
                    					modifyIORef game (const (getNextState tmp))
                    					putStrLn ("bestmove " ++ mv)
                dialogue

    --                                  Just cmd -> do 
    --                              responses <- getResponse cmd
    --                              let output = intercalate "\n" $ map show responses
    --                              appendToFile logFilePath "Engine:"
    --                              appendToFile logFilePath output
    --                              appendToFile lastGamePath output
    --                              putStrLn output
    --            case parseCommand line of
    --                Nothing -> return ()
    --                Just (CmdPosition gs) -> do
    --                                      writeToFile lastGamePath line
    --                _ -> return ()
    --            dialogue
    --            where
    --                getResponse CmdUci = return [RspId "name" "HaskellChess", RspId "author" "Aditya-Shivaram-Nitin-Anish-Ruturaj-Chirag", RspUciOk]
    --                getResponse CmdIsReady = return [RspReadyOk]
    --                getResponse CmdUciNewGame = return []
    --                getResponse CmdQuit = exitSuccess
    --                getResponse CmdStop = return []
    --                getResponse (CmdPosition gs) = do
    --                  modifyIORef lastGameState (const gs)
    --                  --displayGame gs
    --                  return []
    --                getResponse (CmdGo _) = do
    --                  g <- readIORef lastGameState
    --                  -- (pv, p') <- runSearch (search 4) p
    --                  -- writeIORef lastPosition p'
    --                  let	bs = getNextState g openingBook
    --                  let m = genMovesString (fst g) bs
    --                  return [ RspBestMove m ]
    dialogue

	