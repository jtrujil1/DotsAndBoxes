
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
type Board = ([Line], PlayerScores, Player)
 
--Possible Functions
--createBoard
--updateBoard
--AllDots
--ValidBox
--ValidDot
--remove (remove dots/lines that have already been played)
--

--check if board is full
checkBoard :: Bool
checkBoard = undefined 

--creates original board at start //start with 5 x 5
createBoard :: Board
createBoard = undefined

--Update board after each move[line] is made
updateBoard :: Board -> Move -> Board
updateBoard = undefined

--stores list of possible dots to be made
--allDots :: [Dot]
allDots = [(x,y)| x <- [0..5], y <- [0..5]]

--lists of current lines on board after each move
--currentLines :: [Line]
--curentLines = undefined

--remove dots/lines from lists
remove :: Eq a => a -> [a] -> [a]
remove = undefined

--checks to see if line is valid
--error if not valid
validLine :: Line -> Bool
validLine = undefined

--checks to see if box has been made already
validBox:: Box -> Bool
validBox = undefined

--depends if our board is holding the moves done or moves left
validMoves :: Board -> [Move]
validMoves = undefined

--checks highest number of box to declare winner
winner :: Board -> Player
winner (_, (boxes1, boxes2), _) =
   let scores = [(length boxes1, Player1), (length boxes2, Player2)]
   in snd $ maximum scores

--takes in a box and updates the player scores
updateScore :: Player -> Box -> PlayerScores
updateScore = undefined

--turn input into dots for lines
stringTurn :: String -> String -> Maybe Line 
stringTurn = undefined 

--check if line is valid
--check if new box is formed
--update the board
--give back the player who's next
--maybe ? give back tuple with player and score
makePlay :: Player -> Line -> Player
makePlay = undefined

--create a string that show the current state of the game
prettyShow :: Board -> String
prettyShow = undefined

{-
maybe have a matrix of values??
str = "Scores\nPlayer1: " ++ show (length boxes1) ++ "\tPlayer2: " ++ show (length boxes2) ++ "\n"
boardStr = "*-------*" -- 7 are one tab
-}

