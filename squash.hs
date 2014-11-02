import Data.List
import Data.Ord

-- TODO:-
-- handle no valid moves
-- evaluate: determine winner by num moves available?
-- what happens if it's a winning move?
-- clean up makeMove since so similar to minimax

-- x is the current board as a string, xs is a list of
-- all the previous boards as strings
-- n is the board size (length of one side of the hexagonal board)
crusher :: [String] -> Char -> Int -> Int -> [String]
crusher boardStrings player depth n =
   unparse (makeMove boards player depth):boardStrings
   where boards = map (parse n) boardStrings
-----------------MiniMax--------------------------------------------------
-- White is max player, black is min player

-- Produces the board that we determine to be the result of best move from minimax, man!

makeMove :: [Board] -> Char -> Int -> Board
makeMove boards player depth =
   head (fst $ minOrMaxBy (comparing snd) scoredBoards)
   where
      nexts = map (\b -> b:boards) $ getNextBoardsForPlayer boards player
      scores = map (\b -> (minimax b player depth)) nexts
      scoredBoards = zip nexts scores
      minOrMaxBy = if player == 'W' then maximumBy else minimumBy

-- can we assume depth >= 0?
minimax :: [Board] -> Char -> Int -> Int
minimax (board:history) player depth
   | depth == 0   = evaluate board
   | otherwise    = minOrMax player scores
   where
      -- The scores of all the subboards from this board
      scores :: [Int]
      scores  =
         map getNextMinimax possibilites
         where
            getNextMinimax possibility = minimax possibility (other player) (depth-1)
            possibilites =
               map (\b -> b:board:history) (getNextBoardsForPlayer (board:history) player)

-- produce the max if W, and the min if B
minOrMax :: Char -> [Int] -> Int
minOrMax 'W' = maximum
minOrMax 'B' = minimum

-- Switch player
other :: Char -> Char
other 'W' = 'B'
other 'B' = 'W'


---------------- Static Evaluation ----------------------------------------
evaluate :: Board -> Int
evaluate board
    | didWhiteWin      =  3 * size
    | didBlackWin      = -3 * size
    | otherwise = whiteCount - blackCount  -- +  (myMoves - oppMoves)
    where
      size = n board
      didWhiteWin = blackCount < size
      didBlackWin = whiteCount < size
      whiteCount  = length $ whites board
      blackCount  = length $ blacks board

---------------- Parsing ---------------------------------------------------
-- converts input string to our board representation
parse :: Int -> String -> Board
parse n string = Board { whites = filterPlayer 'W' positions,
                         blacks = filterPlayer 'B' positions,
                         n = n
                         }
   where positions = getAllPositions string n

filterPlayer :: Char -> [(Pos, Char)] -> [Pos]
filterPlayer player positions = map fst $ filter is_player positions
-- filterPlayer player positions = [fst p | p <- positions, is_player p]
   where is_player (pos,char) = char == player

getAllPositions :: String -> Int -> [(Pos,Char)]
getAllPositions string n =
   (getTopPositions string n) ++ (getBottomPositions string n)

getTopPositions string n = getTopPositions' string 1 n
getBottomPositions string n =
   getBottomPositions' (drop top_length string) (n+1) n
   where top_length = sum [n..(2*n-1)]


-- for the first n rows
getTopPositions' :: String -> Int -> Int -> [(Pos, Char)]
getTopPositions' string row n
   | row > n = []
   | otherwise = positions ++ (getTopPositions' rest_string (row+1) n)
   where row_indices = repeat row
         row_length = n + row - 1
         col_indices = take row_length [1..]

         pos_list = zip row_indices col_indices
         positions = zip pos_list string

         rest_string = drop row_length string

-- row startCol endCol

-- for the rest of the rows (the next n-1 rows)
-- must pass in the string with the first n rows removed
-- this is the original string minus the first n^2 + n^2/2 - n characters
getBottomPositions' :: String -> Int -> Int -> [(Pos, Char)]
getBottomPositions' string row n
   | row > 2*n-1 = []
   | otherwise = positions ++ (getBottomPositions' rest_string (row+1) n)
   where row_indices = repeat row
         row_length = n + (2*n-1) - row
         col_start = row-n+1
         col_indices = take row_length [col_start..]

         pos_list = zip row_indices col_indices
         positions = zip pos_list string

         rest_string = drop row_length string

-- converts our representation to output string
unparse :: Board -> String
unparse board = map (charAtPosOn board) (getValidPos board)

-- produce character representation of contents of pos on board
charAtPosOn :: Board -> Pos -> Char
charAtPosOn board pos
  | elem pos (whites board)   = 'W'
  | elem pos (blacks board)   = 'B'
  | otherwise                 = '-'

-- Produce all valid Pos in given board. They are in the same order that they appear
-- in an input string.
getValidPos :: Board -> [Pos]
getValidPos board =
   [(r,c) | r <- indices, c <- indices, isValidPosForSize size (r,c)]
   where
      size = (n board)
      indices = [1..2*size-1]

isValidPosForSize :: Int -> Pos -> Bool
isValidPosForSize n pos = isValidRow && isValidCol
   where
      row = fst pos; col = snd pos
      isValidRow = 0 < row && row < 2*n
      isValidCol
         | row < n   = 0 < col && col < n+row
         | otherwise = row-n < col && col < 2*n


----------------- Generating New Boards ---------------------------------

-- For given history of boards, produce all legal boards 1 move away for given player
getNextBoardsForPlayer :: [Board] -> Char -> [Board]
getNextBoardsForPlayer (latest:history) player =
   filter isNew $ getPotentialNextBoards latest player
   where
      isNew = flip notElem history


getPotentialNextBoards :: Board -> Char-> [Board]
getPotentialNextBoards board player =
   map (makeMovedBoard board) (getPlayerMoves board player)

-- Gets all the moves the given player could make on the given board
getPlayerMoves :: Board -> Char -> [Move]
getPlayerMoves board player = concatMap (getPieceMoves board player) pieces
   where pieces = fst $ selectPieces board player

-- Produce (player's pieces, opponent's pieces)
selectPieces :: Board -> Char -> ([Pos], [Pos])
selectPieces board 'W' = (whites board, blacks board)
selectPieces board 'B' = (blacks board, whites board)

-- Produce all moves the player can make with the piece at pos on the board
getPieceMoves :: Board -> Char -> Pos -> [Move]
getPieceMoves board player pos =
  concatMap (getMovesInDir board player pos) dirs

-- HACKED ON THE BUS: BEWARE:


-- Produce all legal moves in given direction for constraints
getMovesInDir :: Board -> Char -> Pos -> Dir -> [Move]
getMovesInDir board player pos dir
  | isValid oneAway && isFree oneAway =               -- slide
    [Move {source=pos, dest=oneAway, mover=player}]
  | isValid twoAway && not (isPlayers twoAway) && isPlayers oneAway = -- jump
    [Move {source=pos, dest=twoAway, mover=player}]
  | otherwise = []
  where
    oneAway = getNeighbour pos dir
    twoAway = getNeighbour oneAway dir
    (players, opponents) = selectPieces board player

    isValid = isValidPosForSize $ n board

    isFree p = notElem p players && notElem p opponents
    isPlayers p = elem p players

-- Produce the pos of the closest neighbour in the given direciton.
-- NOTE: this pos might NOT actually exist
getNeighbour :: Pos -> Dir -> Pos
getNeighbour (r,c) (dr, dc) = (r+dr, c+dc)

makeMovedBoard :: Board -> Move -> Board
makeMovedBoard board move
   | player == 'W'   = Board {whites = newPlayerPieces, blacks = newOpponentPieces, n = (n board)}
   | otherwise       = Board {whites = newOpponentPieces, blacks = newPlayerPieces, n = (n board)}
   where
      player = mover move
      (playerPieces, opponentPieces) = selectPieces board player
      newPlayerPieces = dest move:delete (source move) playerPieces
      newOpponentPieces = delete (dest move) opponentPieces


------------------GRAPHICAL-----------------------


-- Prints all given boards with a blank line between the boards
-- printBoards :: [Board] -> IO ()
-- printBoards boards = mapM_ (\ b -> printBoard ("":b)) boards

-- Shows the given board
printRows :: [String] -> IO ()
printRows rows = putStr (unlines rows)

splitBoard bstring = ["  " ++ row1, " " ++ row2, row3, " " ++ row4, "  " ++ row5]
   where
    b = intersperse ' ' bstring
    row1 = take 6 b
    row2 = take 8 (drop 6 b)
    row3 = take 10 (drop 14 b)
    row4 = take 8 (drop 24 b)
    row5 = take 6 (drop 32 b)

printBoard b = printRows (splitBoard b)

-- -- Prints all given boards with a blank line between the boards
-- -- printBoards :: [B] -> IO ()
-- printBoards boards = mapM_ (\ b -> printBoard ([]:b)) bs
--    where bs = reverse (map splitBoard boards)

-- play :: Int -> [String]
play turns d p = play' turns [[string3]] p d
play' 0 history _ _ = head history
play' turns (x:xs) p d =
   play' (turns-1) ((crusher x p d 3):x:xs) (other p) d




-------------- data types -----------------------------------------

data Board = Board {whites :: [Pos],
                    blacks :: [Pos],
                    n :: Int
                    }
             deriving (Show,Eq)

-- (Row, Column), 1-indexed, with 1 in top left;
type Pos = (Int, Int)

data Move = Move {source :: Pos,
                  dest :: Pos,
                  mover :: Char
                  }
            deriving (Show,Eq)

-- TODO Alison check directions are correct            
-- The closest neighbour in a given direction, with (drow, dcol)
type Dir = (Int, Int)

nw = (-1, -1); ne = (-1, 0)
w =  ( 0, -1); e  = ( 0, 1)
sw = ( 1,  0); se = ( 1, 1)
dirs = [nw,ne,e,se,sw,w] -- the directions in clockwise order















----------- examples -----------------------------


data Pos2 = Pos2 {row :: Int,
                  col :: Int
                  }
            deriving (Show,Eq)

pos1 = (1,1)
pos2 = (2,2)
pos2_1 = Pos2 {row = 1, col = 1}
pos2_2 = Pos2 {row = 2, col = 2}



board1 = Board {whites = [pos1,pos2], blacks = [], n = 3}
string3 = "WWW-WW-------BB-BBB"
board3 = parse 3 string3
string4 = "WWWW-WWW---WW-----------BB---BBB-BBBB"  -- just guessing ...
board4 = parse 4 string4

split3 bstring = ["  " ++ row1, " " ++ row2, row3, " " ++ row4, "  " ++ row5]
   where
    b = intersperse ' ' bstring
    row1 = take 6 b
    row2 = take 8 (drop 6 b)
    row3 = take 10 (drop 14 b)
    row4 = take 8 (drop 24 b)
    row5 = take 6 (drop 32 b)


-- play :: Int -> [String]
play turns d p = play' turns [[string3]] p d
play' 0 history _ _ = head history
play' turns (x:xs) p d =
   play' (turns-1) ((crusher x p d 3):x:xs) (other p) d    

playIt turns d p = reverse (map split3 (play turns d p))   

-- Get a list of lists of strings and output them nicely.      
printStrMatrix :: [[String]] -> IO ()        
printStrMatrix [] = printStrList []
printStrMatrix (x:xs) = do
        printStrList x
        printStrMatrix xs

-- Print nicely a list of strings.
-- Eg: printStrList ["aabb", "ccdd", "eeff"] prints in the console:
-- aabb
-- ccdd
-- eeff
printStrList :: [String] -> IO ()
printStrList [] = putStrLn ""
printStrList (x:xs) = do 
        putStrLn x
        printStrList xs            

  
