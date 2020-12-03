
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Text.Read (readMaybe)
import Data.Char (toUpper)

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
bestOutcome lst player = 
   let losses = [m | (o, m) <- lst, o == (Winner (opponent player))]
   in  case lookup (Winner player) lst of
             Nothing -> case lookup Tie lst of 
                             Nothing -> Just $ head losses
                             move -> move
             move -> move

readGame :: String -> Maybe Game
readGame str =
   case splitOn "\n" str of
      [sizeStr, boardStr, p1Str, p2Str, playerStr] ->
         do let unStr str = sequence [readMaybe x | x <- splitOn "." str] 
            size <- readMaybe sizeStr
            board <- unStr boardStr 
            p1 <- unStr p1Str 
            p2 <- unStr p2Str 
            player <- case playerStr of
                           "1" -> Just Player1
                           "2" -> Just Player2
                           _ -> Nothing
            return (size, board, (p1, p2), player)
      _ -> Nothing

showGame :: Game -> String
showGame game@(size, board, (p1, p2), player) =
   let str lst = intercalate "." $ map show lst
       p = case player of
                  Player1 -> "1"
                  Player2 -> "2"
   in intercalate "\n" [show size, str board, str p1, str p2, p]


writeGame :: Game -> String -> IO ()
writeGame game fileName = 
   let gameStr = showGame game
   in writeFile fileName gameStr
       
loadGame :: FilePath -> IO (Maybe Game)
loadGame fp =
   do str <- readFile fp
      return $ readGame str

putWinner :: Game -> IO ()
putWinner game =
   let winnerStr = case whoWillWin game of
                     Winner Player1 -> "Player1"
                     Winner Player2 -> "Player2"
                     Tie -> "T"
   in do case winnerStr of
                 "T" -> putStrLn $ "It's a tie!!!"
                 otherwise -> putStrLn $ "And the winner is ... " ++ (map (toUpper) winnerStr) ++ "!!!"

eval :: Game -> Int
eval game@(size, board, (p1, p2), player) = 
   case checkBoard game of 
      GameOver Tie -> 0
      GameOver (Winner Player1) -> (size-1)^2 + 1
      GameOver (Winner Player2) -> negate $ (size-1)^2 + 1
      Ongoing -> 
          let (score1, score2) = (length p1, length p2)
          in max score1 score2

--like bestMove but limited by the depth
--not sure if we should be calling whoWillMaybeWin with (depth-1) or just depth haven't checked
goodMove :: Game -> Int -> Maybe Move
goodMove game@(size, board, (p1, p2), player) depth =
  let vMoves = validMoves game
      futurePlays = zip vMoves (catMaybes [makeMove game move | move <- vMoves])
      evals = [(whoWillMaybeWin newGame (depth-1), move)| (move, newGame) <- futurePlays]
  in if null vMoves then Nothing else Just (bestEval evals player)

--chooses the best move depending on whose player's turn it is
bestEval :: [(Int, Move)] -> Player-> Move
bestEval lst player = 
   case player of
        Player1 -> snd $ maximum lst
        Player2 -> snd $ minimum lst

--like whoWillWin but limited by depth
whoWillMaybeWin :: Game -> Int -> Int 
whoWillMaybeWin game@(size, board, scores, player) depth = 
  case checkBoard game of 
      GameOver outcome -> eval game
      Ongoing -> 
        let vMoves = validMoves game
            futurePlays = zip vMoves (catMaybes [makeMove game move | move <- vMoves])
            evals = [whoWillMaybeWin newGame (depth-1) | (move, newGame) <- futurePlays]
        in if depth <= 0 then 0 else chooseEval evals player

--chooses the best outcome depending on whose player's turn it is
chooseEval :: [Int] -> Player -> Int
chooseEval lst player =
   case player of
        Player1 -> maximum lst
        Player2 -> minimum lst


{-
Player: Player1
Score : 5
===================
*   *   *   *   

*   *   *   *   

*   *   *   *   

*   *   *   *   
===================

--}
