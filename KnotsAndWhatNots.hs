
import Data.Tuple (swap)
import Data.List
import Data.List.Split
--import Data.List (nub)
import Data.Maybe
import Debug.Trace

--Data Types--
type Dot = (Int, Int)
type Line = (Dot, Dot)--((1,1),(4,5))
type Box = Dot
type Move = Line 
data Player = Player1 | Player2 deriving (Show, Eq, Ord)
type PlayerScores = ([Box],[Box])
data Outcome = Winner Player | Tie deriving (Show, Eq, Ord)
data GameState = Ongoing | GameOver Outcome deriving (Show, Eq, Ord)
type Board = (Int, [Line], PlayerScores, Player)


--Possible Functions
--createBoard
--updateBoard
--AllDots
--ValidBox
--ValidDot
--remove (remove dots/lines that have already been played)

allDots size = [(x,y)| x <- [0..size-1], y <- [0..size-1]]

allBoxes size = [(x,y)| x <- [0..size-2], y <- [0..size-2]]

allLines size = [((a,b),(a+1,b)) | a <- [0..size-1], b <- [0..size-1], not (a+1 == size)] ++ [((a,b), (a,b+1)) | a <- [0..size-1], b <- [0..size-1], not (b+1 == size)]

--turn input into dots for lines
readStr :: String -> String -> Maybe Line 
readStr = undefined 

--creates original board at the beginning
--create orignal lines left on board
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

--check if line is valid
--rm for list in board
--check if new box is formed
--update the board
--give back the player who's next
--maybe ? give back tuple with player and score
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
--check with Fogarty

winner :: Board -> Outcome
winner (size, board, (boxes1, boxes2), _) =
   let scores@[(score1, p1), (score2, p2)] = [(length boxes1, Player1), (length boxes2, Player2)]
       champ = snd $ maximum scores
   in if score1 == score2 then Tie else Winner champ

{-  if state == GameOver
              then Nothing-}

--create a string that show the current state of the game
--[((0,0),(1,0)),((0,1),(1,1)),((0,2),(1,2)),((1,0),(2,0)),((1,1),(2,1)),((1,2),(2,2)),((0,0),(0,1)),((0,1),(0,2)),((1,0),(1,1)),((1,1),(1,2)),((2,0),(2,1)),((2,1),(2,2))]
--((a,b),(c,d))
prettyShow :: Board -> String
prettyShow (size, board, (box1, box2), player) = 
  let horz =  [line | line <- allLines size, horizontal line]
      vertz = [line | line <- allLines size, not (horizontal line)] 
      played = filter (\x -> x `notElem` board) (allLines size)
      str = "Scores\nPlayer1: " ++ show (length box1) ++ "\tPlayer2: " ++ show (length box2) ++ "\n"
      border = "==============="
      lineStr [] = []
      lineStr (x:xs) = (lineToString x played size) ++ lineStr xs
  in border ++ str ++ border -- + lineStr lines
--((x,y),(a,b))
lineToString :: Line -> [Line] -> Int -> String
lineToString line@((_,_), (x,y)) played size =
  let horLineStr = if line `elem` played then "---" else "   "
      vertLineStr = if line `elem` played then "|  " else "   "
      endOfHorLine = if y == size-1 then "*\n" else ""
      endOfVertLine = if y == size-1 then "\n" else ""
  in if horizontal line
        then "*" ++ horLineStr ++ endOfHorLine
        else vertLineStr ++ endOfVertLine

{-
putStrLn $ prettyShow
--maybe have a matrix of values??
str = "Scores\nPlayer1: " ++ show (length boxes1) ++ "\tPlayer2: " ++ show (length boxes2) ++ "\n"
boardStr = "*-------*" -- 7 are one tab
Player: Player1
Score : 5
===================
*   *   *   *   *

*   *   *   *   *

*   *   *   *   *

*   *   *   *   *
===================

--}





