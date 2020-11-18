
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
type Board = (Int, [Line], PlayerScores, Player)

allDots size = [(x,y)| x <- [0..size-1], y <- [0..size-1]]

allBoxes size = [(x,y)| x <- [0..size-2], y <- [0..size-2]]

allLines size = [((x,y), True) | x <- [0..size-2], y <- [0..size-1]] ++ [((x,y), False) | x <- [0..size-1], y <- [0..size-2]]

--turn input into dots for lines
readStr :: String -> String -> Maybe Line 
readStr = undefined 

--creates original board at the beginning
createBoard :: Int -> Board
createBoard size =
   let lines = allLines size
   in (size, lines, ([],[]), Player1)

--check if board is full
checkBoard :: Board -> GameState
checkBoard game@(_, board, _, _) = if null board then GameOver (winner game) else Ongoing

--remove line from board
updateBoard :: Eq a => a -> [a] -> [a]
updateBoard _ [] = []
updateBoard line (x:xs) = if x == line then xs else [x] ++ updateBoard line xs

--checks to see if box is valid
validBox:: Int -> PlayerScores -> Box -> Bool
validBox size (p1, p2) (x,y) = (x >= 0 && x < size-1) && (y >= 0 && y < size-1) && ((x,y) `notElem` (p1++p2))

--depends if our board is holding the moves done or moves left
validMoves :: Board -> [Move]
validMoves (_, lines, _,_) = lines

makeMove :: Board -> Move -> Maybe Board
makeMove game@(size, board, scores, player) line =
   let valid = line `elem` board
       newBoard = updateBoard line board
       boxes = validNewBoxes (size, newBoard, scores, player) line
   in if valid then Just (updateScores (size, newBoard, scores, player) boxes) else Nothing

validNewBoxes :: Board -> Move -> [Box]
validNewBoxes (size, board, scores, player) ((x,y), dir) =
   let possibleBoxes = if dir
                            then filter (validBox size scores) [(x,y),(x,y-1)]
                            else filter (validBox size scores) [(x,y),(x-1,y)]
       played ln = (ln `notElem` board) 
       playedBox (l1, l2, l3, l4) = played l1 && played l2 && played l3 && played l4
   in  [box | box <- possibleBoxes, playedBox (linesOfBox box)]

linesOfBox :: Box -> (Line, Line, Line, Line)
linesOfBox (x,y) = (((x,y), True),((x,y), False),((x,y+1), True),((x+1,y), False))

updateScores :: Board -> [Box] -> Board
updateScores (size, board, (p1, p2), player) boxes =
    let noBoxes = null boxes
    in case player of
            Player1 -> if noBoxes then (size, board, (p1, p2), Player2) else (size, board, (p1++boxes, p2), Player1)
            Player2 -> if noBoxes then (size, board, (p1, p2), Player1) else (size, board, (p1, p2++boxes), Player2)

--checks highest number of box to declare winner
--maybe it should return a Maybe outcome???????
winner :: Board -> Outcome
winner (size, board, (boxes1, boxes2), _) =
   let scores@[(score1, p1), (score2, p2)] = [(length boxes1, Player1), (length boxes2, Player2)]
       champ = snd $ maximum scores
   in if score1 == score2 then Tie else Winner champ

--create a string that show the current state of the game
prettyShow :: Board -> String
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

{-
bestPlay:: Dictionary -> Hand -> Play 
bestPlay dict [] = []
bestPlay dict hand =
   let vMoves = validMoves dict hand
       plays = [move:(bestPlay vMoves (updateHand hand move)) | move <- vMoves]
       scoreValidPlays = [(scorePlay play, play) | play <- plays, isValidPlay vMoves hand play]
   in  if null vMoves then [] else snd (maximum scoreValidPlays)
-}

--given current board state, calculate future moves
whoWillWin :: Board -> Outcome
whoWillWin game@(size, board, scores, player) =
   let vMoves = validMoves game
       futurePlays = concatMaybes [makeMove game move | move <- vMoves] --[boards(valid moves)]
       state = [checkBoard game | game <- futurePlays ]
       outcomes = [(whowillWin game, x) | (x,game) <- plays vMoves] --check outof future plays -> a finished gamestate && where player# wins/ties
   in if state == GameOver then [] else Just snd (chooseOutcome outcomes player (head outcomes))

{-
-- Full credit Maybe Move
bestMove :: Board -> Maybe Move
bestMove game@(size, board, scores, player) = 
   let vMoves = validMoves game
       plays [] _ = []
       plays (x:xs) currentGame = 
          let updatedGame = makeMove currentGame x
          in case updatedGame of
                          Nothing -> Nothing
                          Just newGame -> (x, updatedGame):(bestMove vMoves updatedGame) ++ plays xs updatedGame
       outcomes = [(whowillWin game, x) | (x,game) <- plays vMoves]
       state = checkBoard game
   in if state == GameOver then [] else Just snd (chooseOutcome outcomes player (head outcomes))

chooseOutcome [] _ best = best
chooseOutcome (x:xs) player (best, move) = if best == Winner player
                                              then (best, move)
                                              else []

{-
readGame :: String -> Game (Full Credit: Maybe Game)
readGame = undefined

showGame :: Game -> String
showGame = undefined

writeGame :: Game -> String -> IO ()
writeGame = undefined

readGame :: String -> IO Game (Full Credit: IO (Maybe Game))
readGame = undefined

putWinner :: Game -> IO ()
putWinner = undefined
-}

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
