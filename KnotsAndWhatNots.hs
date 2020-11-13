
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

--Data Types--
type Dot = (Int, Int)
type Line = (Dot, Dot)--((1,1),(4,5))
type Box = Dot
type Move = Line 
data Player = Player1 | Player2 deriving (Show, Eq, Ord)
type PlayerScores = ([Box],[Box])
data GameState = Ongoing | GameOver deriving (Show, Eq, Ord)
type Board = ([Line], PlayerScores, Player, GameState)


--Possible Functions
--createBoard
--updateBoard
--AllDots
--ValidBox
--ValidDot
--remove (remove dots/lines that have already been played)

size = 5

allDots = [(x,y)| x <- [0..size-1], y <- [0..size-1]]

allBoxes = [(x,y)| x <- [0..size-2], y <- [0..size-2]]

lineMaker [] = []
lineMaker ((a,b):xs) = [((a,b), (c,d)) | (c,d) <- xs, ((a+b) - (c+d))^2 == 1, (dist (a,b) (c,d)) == 1] ++ lineMaker xs

allLines = lineMaker allDots

dist (x1, y1) (x2, y2) =  ceiling $ sqrt $ (fromIntegral x1 - fromIntegral x2)^2 + (fromIntegral y1 - fromIntegral y2)^2

--Box [((x,y)(x+1, y)),((x,y)(x, y+1)), ((x+1,y)(x+1, y+1)), ((x,y+1)(x+1, y+1))]

--lists of current lines on board after each move
--currentLines :: [Line]
--curentLines = undefined

--turn input into dots for lines
readStr :: String -> String -> Maybe Line 
readStr = undefined 

--creates original board at the beginning
--create orignal lines left on board
createBoard :: Board
createBoard = (allLines, ([],[]), Player1, Ongoing)

--Update board after each move is made
--check amount of boxes made
updateBoard :: Board -> Move -> Board
updateBoard = undefined

--check if board is full
checkBoard :: GameState
checkBoard = undefined 

--remove dots/lines from lists
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove line (x:xs) = if x == line then xs else [x] ++ remove line xs

--checks to see if line is valid
-- maybe it's redundant cause we already have validMoves
{-
validLine :: Line -> [Line] -> Bool
validLine line board = line `elem` boar
-}

--checks to see if box is valid
validBox:: Box -> PlayerScores -> Bool
validBox box (p1, p2) = (box `elem` allBoxes) && not $ box `elem` (p1++p2) 

--depends if our board is holding the moves done or moves left
validMoves :: Board -> [Move]
validMoves (board, _,_,_) = board

--check if line is valid
--rm for list in board
--check if new box is formed
--update the board
--give back the player who's next
--maybe ? give back tuple with player and score

makeMove :: Board -> Move -> Maybe Board
makeMove (board, scores, player,state) line =
    let valid = line `elem` validMoves (board, scores, player,state)
        newBoard = remove line board
        newBox = checkNewBox line scores
        newPlayer = if player == Player1 then Player2 else Player1
    in if valid then Just updateBoard (newBoard, newBoxes, newPlayer, state) else Nothing

--return bool or??
checkNewBox ((x1,y1), (x2,y2)) scores = 
    let boxes = if horizontal ((x1,y1), (x2,y2)) then filter (\x -> validBox x scores) [(x1,y1),(x1,y1-1)] else filter (\x -> validBox x scores) [(x1,y1),(x1-1,y1)]
       bLines = 
    in 

horizontal ((x1,y1), (x2,y2)) = y1 == y2

--takes in a box and updates the player scores
updateScore :: Player -> Box -> PlayerScores
updateScore = undefined

--checks highest number of box to declare winner
--check with Fogarty
winner :: Board -> Board
winner (board, (boxes1, boxes2), _,_) =
   let scores = [(length boxes1, Player1), (length boxes2, Player2)]
       champ = snd $ maximum scores
   in (board,(boxes1, boxes2),champ,GameOver)

{-  if state == GameOver
              then Nothing-}

--create a string that show the current state of the game
--prettyShow :: Board -> String
--prettyShow = undefined

{-
maybe have a matrix of values??
str = "Scores\nPlayer1: " ++ show (length boxes1) ++ "\tPlayer2: " ++ show (length boxes2) ++ "\n"
boardStr = "*-------*" -- 7 are one tab
-}

