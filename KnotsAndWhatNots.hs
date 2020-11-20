
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

--Data Types--
type Dot = (Int, Int)
type Line = (Dot, Bool) -- True means it is horizontal
type Box = Dot
type Move = Line 
data Player = Player1 | Player2 deriving (Show, Eq, Ord)
type PlayerScores = ([Box],[Box])
data Outcome = Winner Player | Tie deriving (Show, Eq, Ord)
data GameState = Ongoing | GameOver Outcome deriving (Show, Eq, Ord)
type Game = (Int, [Line], PlayerScores, Player)


allDots size = [(x,y)| x <- [0..size-1], y <- [0..size-1]]

allBoxes size = [(x,y)| x <- [0..size-2], y <- [0..size-2]]

allLines size = [((x,y), True) | x <- [0..size-2], y <- [0..size-1]] ++ [((x,y), False) | x <- [0..size-1], y <- [0..size-2]]

--creates original board at the beginning
createGame :: Int -> Game
createGame size =
   let lines = allLines size
   in (size, lines, ([],[]), Player1)

--check if board is full
checkBoard :: Game -> GameState
checkBoard game@(_, board, _, _) = if null board then GameOver (winner game) else Ongoing

--remove line from board
updateBoard :: Eq a => a -> [a] -> [a]
updateBoard _ [] = []
updateBoard line (x:xs) = if x == line then xs else [x] ++ updateBoard line xs

--checks to see if box is valid
validBox:: Int -> PlayerScores -> Box -> Bool
validBox size (p1, p2) (x,y) = (x >= 0 && x < size-1) && (y >= 0 && y < size-1) && ((x,y) `notElem` (p1++p2))

--depends if our board is holding the moves done or moves left
validMoves :: Game -> [Move]
validMoves (_, lines, _,_) = lines

makeMove :: Game -> Move -> Maybe Game
makeMove game@(size, board, scores, player) line =
   let valid = line `elem` board
       newBoard = updateBoard line board
       boxes = validNewBoxes (size, newBoard, scores, player) line
   in if valid then Just (updateScores (size, newBoard, scores, player) boxes) else Nothing

validNewBoxes :: Game -> Move -> [Box]
validNewBoxes (size, board, scores, player) ((x,y), dir) =
   let possibleBoxes = if dir
                            then filter (validBox size scores) [(x,y),(x,y-1)]
                            else filter (validBox size scores) [(x,y),(x-1,y)]
       played ln = (ln `notElem` board) 
       playedBox (l1, l2, l3, l4) = played l1 && played l2 && played l3 && played l4
   in  [box | box <- possibleBoxes, playedBox (linesOfBox box)]

linesOfBox :: Box -> (Line, Line, Line, Line)
linesOfBox (x,y) = (((x,y), True),((x,y), False),((x,y+1), True),((x+1,y), False))

updateScores :: Game -> [Box] -> Game
updateScores (size, board, (p1, p2), player) boxes =
    let noBoxes = null boxes
    in case player of
            Player1 -> if noBoxes then (size, board, (p1, p2), Player2) else (size, board, (p1++boxes, p2), Player1)
            Player2 -> if noBoxes then (size, board, (p1, p2), Player1) else (size, board, (p1, p2++boxes), Player2)

--checks highest number of box to declare winner
winner :: Game -> Outcome
winner (size, board, (boxes1, boxes2), _) =
   let scores@[(score1, p1), (score2, p2)] = [(length boxes1, Player1), (length boxes2, Player2)]
       champ = snd $ maximum scores
   in if score1 == score2 then Tie else Winner champ

--create a string that show the current state of the game
prettyShow :: Game -> String
prettyShow game@(size, board, (p1, p2), player) = 
   let played = filter (\x -> x `notElem` board) (allLines size)
       boardStr = concat [rowStr num played game | num <- [0..size-1]]
       scoresStr = "Scores\nPlayer1: " ++ show (length p1) ++ "\tPlayer2: " ++ show (length p2) ++ "\n"
       border = "===============\n"
   in border ++ scoresStr ++ border ++ boardStr ++ border

rowStr num played (size, _, (p1, p2), _) =
    let hor = [((x, num), True) | x <- [0..size-2]] 
        ver = [((x, num), False) | x <- [0..size-1]]
        horStr [] = []
        horStr (x:xs) = (horizontalLine x played size) ++ horStr xs
        verStr [] = []
        verStr (x:xs) = (verticalLine x played size (p1,p2)) ++ verStr xs
    in  (horStr hor) ++ "*\n" ++ (verStr ver) ++ "\n"

horizontalLine line played size = if line `elem` played then "*---" else "*   "

verticalLine line@(dot, False) played size (p1, p2) =
   let str = if line `elem` played then "|" else " "
       num
          | dot `elem` p1 = " 1 "
          | dot `elem` p2 = " 2 "
          | otherwise = "   "
   in str ++ num

whoWillWin :: Game -> Outcome
whoWillWin game@(size, board, scores, player) = 
  case checkBoard game of 
      GameOver outcome -> outcome
      Ongoing -> 
        let vMoves = validMoves game
            futurePlays = catMaybes [makeMove game move | move <- vMoves]
            outcomes = [whoWillWin newGame| newGame <- futurePlays]
        in chooseOutcome outcomes player

chooseOutcome :: [Outcome] -> Player -> Outcome
chooseOutcome outcomes player
   | Winner player `elem` outcomes = Winner player 
   | Tie `elem` outcomes = Tie
   | otherwise = Winner (opponent player)

opponent Player1 = Player2
opponent Player2 = Player1

bestMove :: Game -> Maybe Move 
bestMove game@(size, board, scores,player) = 
  let vMoves = validMoves game
      futurePlays = zip vMoves (catMaybes [makeMove game move | move <- vMoves])
      outcomes = [(whoWillWin newGame, move)| (move, newGame) <- futurePlays]
  in if null vMoves then Nothing else (bestOutcome outcomes player)

bestOutcome :: [(Outcome, Move)] -> Player-> Maybe Move
bestOutcome lst player = case lookup (Winner player) lst of
                              Nothing -> case lookup Tie lst of 
                                         Nothing -> case lookup (Winner (opponent player)) lst of 
                                                    Nothing -> Nothing
                                                    move -> move
                                         move -> move
                              move -> move

readGame :: String -> Maybe Game --(Full Credit: Maybe Game)
readGame str =
   let [sizeStr, boardStr, p1Str, p2Str, playerStr] = splitOn "\n" str
       size = read sizeStr
       unStr str = [read x | x <- splitOn "." str]
       board = unStr boardStr 
       p1 = unStr p1Str 
       p2 = unStr p2Str 
       player = case playerStr of
                       "1" -> Player1
                       "2" -> Player2
   in if length (splitOn "\n" str) == 5 then Just (size, board, (p1, p2), player) else Nothing

showGame :: Game -> String
showGame game@(size, board, (p1, p2), player) =
   let str lst = intercalate "." ( map show lst )
       p = case player of
                  Player1 -> "1"
                  Player2 -> "2"
   in intercalate "\n" [show size, str board, str p1, str p2, p]


writeGame :: Game -> String -> IO ()
writeGame game fileName = 
   let gameStr = showGame game
   in writeFile fileName gameStr
       
loadGame :: FilePath -> IO Game
loadGame fp =
   do str <- readFile fp
      case readGame str of
              Just game -> return game 
              Nothing -> return (createGame 3) --Ask Fogarty what this is
{-
Also do we need this one too??????????
readGame :: String -> IO Game --(Full Credit: IO (Maybe Game))
readGame str = case readGame str of
                 Just game -> putStrLn $ prettyShow game
                 Nothing -> createBoard 3
-}

--call whoWillWin function but make it pretty
putWinner :: Game -> IO ()
putWinner game =
   let winnerStr = case whoWillWin game of
                     Winner player -> case player of
                                           Player1 -> "Player1"
                                           Player2 -> "Player2"
                     Tie -> "Tie"
   in do case winnerStr of
                 "Tie" -> putStrLn $ "It's a tie"
                 otherwise -> putStrLn $ "And the winner is ... " ++ winnerStr ++ "!!!"


{-
Player: Player1
Score : 5
===================
*   *   *   *   *

*   *   *   *   *

*   *   *   *   *

*   *   *   *   *
===================

--}
