
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

