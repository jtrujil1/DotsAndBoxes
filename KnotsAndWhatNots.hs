
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

--Data Types--
type Dot = (Int, Int)
type Line = (Dot, Dot)
type Box = Dot
type Move = Line 
data Player = Player1 | Player2 deriving (Show, Eq, Ord)
type PlayerScores = ([Box],[Box])
data Outcome = Winner Player | Tie deriving (Show, Eq, Ord)
data GameState = Ongoing | GameOver Outcome deriving (Show, Eq, Ord)
type Board = (Int, [Line], PlayerScores, Player)

allDots size = [(x,y)| x <- [0..size-1], y <- [0..size-1]]

allBoxes size = [(x,y)| x <- [0..size-2], y <- [0..size-2]]

allLines size = [((a,b),(a+1,b)) | a <- [0..size-1], b <- [0..size-1], not (a+1 == size)] ++ [((a,b), (a,b+1)) | a <- [0..size-1], b <- [0..size-1], not (b+1 == size)]

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
validNewBoxes (size, board, scores, player) ((x1,y1), (x2,y2)) =
   let possibleBoxes = if horizontal ((x1,y1), (x2,y2))
                            then filter (validBox size scores) [(x1,y1),(x1,y1-1)]
                            else filter (validBox size scores) [(x1,y1),(x1-1,y1)]
       played ln = (ln `notElem` board) 
       playedBox (l1, l2, l3, l4) = played l1 && played l2 && played l3 && played l4
   in  [box | box <- possibleBoxes, playedBox (linesOfBox box)]

linesOfBox :: Box -> (Line, Line, Line, Line)
linesOfBox (x,y) = (((x,y),(x+1,y)),((x,y), (x, y+1)),((x+1,y), (x+1,y+1)),((x,y+1),(x+1,y+1)))

horizontal ((x1,y1), (x2,y2)) = y1 == y2

updateScores :: Board -> [Box] -> Board
updateScores (size, board, (p1, p2), player) boxes =
    let noBoxes = null boxes
    in case player of
            Player1 -> if noBoxes then (size, board, (p1, p2), Player2) else (size, board, (p1++boxes, p2), Player1)
            Player2 -> if noBoxes then (size, board, (p1, p2), Player1) else (size, board, (p1, p2++boxes), Player2)

--checks highest number of box to declare winner
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
    let hor = [((x1, y1), (x2, y2)) | ((x1, y1), (x2, y2)) <- allLines size, y1 == num, horizontal ((x1, y1), (x2, y2))] 
        ver = [((x1, y1), (x2, y2)) | ((x1, y1), (x2, y2)) <- allLines size, y1 == num, not (horizontal ((x1, y1), (x2, y2)))]
    in  (horStr hor played size) ++ "*\n" ++ (verStr ver played size (p1, p2)) ++ "\n"

horStr [] _ _ = []
horStr (x:xs) played size = (horizontalLine x played size) ++ horStr xs played size

verStr [] _ _ _ = []
verStr (x:xs) played size (p1, p2) = (verticalLine x played size (p1,p2)) ++ verStr xs played size (p1, p2)

horizontalLine line@((_,_), (x,y)) played size = if line `elem` played then "*---" else "*   "


verticalLine line@((x1, y1), (x2,y2)) played size (p1, p2) =
   let str = if line `elem` played then "|" else " "
       num
          | (x1, y1) `elem` p1 = " 1 "
          | (x1, y1) `elem` p2 = " 2 "
          | otherwise = "   "
   in str ++ num

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
