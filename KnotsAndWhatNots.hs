
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

size = 3
--Possible Functions
--createBoard
--updateBoard
--AllDots
--ValidBox
--ValidDot
--remove (remove dots/lines that have already been played)
--

--check if board is full
--check amount of boxes made
checkBoard :: GameState
checkBoard = undefined 

--creates original board at start //start with 5 x 5
--create orignal lines left on board
createBoard :: Board
createBoard = undefined

--Update board after each move[line] is made
updateBoard :: Board -> Move -> Board
updateBoard = undefined

--stores list of possible dots to be made
--allDots :: [Dot]
allDots = [(x,y)| x <- [0..size], y <- [0..size]]

allBoxes = [(x,y)| x <- [1..size -1], y <- [0..size-2]]

--lists of current lines on board after each move
--currentLines :: [Line]
--curentLines = undefined

--remove dots/lines from lists
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove line (x:xs) =  

--checks to see if line is valid
--
validLine :: Line -> Bool
validLine = undefined

--checks to see if box has been made already
validBox:: Box -> Bool
validBox = undefined

--depends if our board is holding the moves done or moves left
validMoves :: Board -> [Move]
validMoves (board, _,_,_) = board

--checks highest number of box to declare winner
--check with Fogarty
winner :: Board -> Board
winner (board, (boxes1, boxes2), _,_) =
   let scores = [(length boxes1, Player1), (length boxes2, Player2)]
       champ = snd $ maximum scores
   in (board,(boxes1, boxes2),champ,GameOver)

--takes in a box and updates the player scores
updateScore :: Player -> Box -> PlayerScores
updateScore = undefined

--turn input into dots for lines
stringTurn :: String -> String -> Maybe Line 
stringTurn = undefined 

--check if line is valid
--rm for list in board
--check if new box is formed
--update the board
--give back the player who's next
--maybe ? give back tuple with player and score
makeMove :: Board -> Move -> Maybe Board
makeMove (board, scores, player,state) line =
  if state == GameOver then Nothing else
  let valid = validLine line
      newBoard = remove line board
      newBoxes = checkBox?
      newPlayer = if player == Player1 then Player2 else Player1
  in if valid then Just updateBoard (newBoard, newBoxes, newPlayer, state) else Nothing 

--create a string that show the current state of the game
--prettyShow :: Board -> String
--prettyShow = undefined

{-
maybe have a matrix of values??
str = "Scores\nPlayer1: " ++ show (length boxes1) ++ "\tPlayer2: " ++ show (length boxes2) ++ "\n"
boardStr = "*-------*" -- 7 are one tab
-}

