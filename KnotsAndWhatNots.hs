
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

--Data Types--
data Board = EmptyBoard | FullBoard deriving Show
data Line = (Dot, Dot) deriving Show
data Dots = (Int, Int)
data Box = (Line, Line, Line, Line) deriving Show
data Player = Player1 Int [Box] | Player2 Int [Box] deriving Show

--Possible Functions
--createBoard
--updateBoard
--AllDots
--ValidBox
--ValidDot
--remove (remove dots/lines that have already been played)
--
--creates original board at start //start with 5 x 5
createBoard :: Board
createBoard = undefined

--Update board after each move[line] is made
updateBoard :: [Dots] -> [Line] -> Board
updateBoard = undefined

--stores list of possible dots to be made
allDots :: [Dot]
allDots dots = [(x,y)| x <- [0..5], y <- [0..5]]

--lists of current dots on board at each move
currentDots :: [Dot]
currentDots = undefined

--lists of current lines on board after each move
currentLines :: [Line]
curentLines = undefined

--checks if box is even possible [formula for lines to make box]
checkLines :: [Line] -> Box
checkLines = undefined

--remove dots/lines from lists
remove :: Eq a => a -> [a] -> [a]
remove = undefined

--checks to see if line is valid
checkLine :: Dot -> Dot ->Bool
checkLine = undefined

--checks to see if box has been made already
validBox :: Box -> Box
validBox = undefined

--checks highest number of box to declare winner
playerWin :: [Box] -> [Box] -> Bool
playerWin = undefined
