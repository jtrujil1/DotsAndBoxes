
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

--Data Types--
type Board = [Line] deriving Show
type Line = (Dot, Dot) deriving Show
data Dots = (Int, Int)
type Box = Dot deriving Show
data Player = Player1 | Player2
data PlayerScores = (P1 (Player, Int, [Box]), P2 (Player, Int, [Box])) deriving Show

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
updateBoard :: [Line] -> Board
updateBoard = undefined

--stores list of possible dots to be made
allDots :: [Dot]
allDots dots = [(x,y)| x <- [0..5], y <- [0..5]]

--lists of current lines on board after each move
currentLines :: [Line]
curentLines = undefined

--remove dots/lines from lists
remove :: Eq a => a -> [a] -> [a]
remove = undefined

--checks to see if line is valid
validLine :: Dot -> Dot -> Bool
validLine = undefined

--checks to see if box has been made already
validBox :: Box -> Bool
validBox = undefined

--checks highest number of box to declare winner
playerWin :: PlayerScores -> Player
playerWin = undefined

--takes in a box and updates the player scores
updateScore :: Player -> Box -> PlayerScores
updateScore = undefined

--check if line is valid
--check if new box is formed
--update the board
--give back the player who's next
makePlay :: Player -> Line -> Player
makePlay = undefined


