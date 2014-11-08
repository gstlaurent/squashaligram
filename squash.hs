import Data.List
import Data.Ord

-------------- Data Types -----------------------------------------------------

-- We've associated each character in the string with a position on the
-- board. The positions are tuples of row and column numbers. Rows and Columns
-- are 1-indexed. The first row has positions (1, 1) to (1, size), where size is
-- the size of the board passed to the crusher function.

-- The rows are horizontal, but the columns are diagonal.
-- For example, a board of size 3 has the following layout:

--       (1,1) (1,2) (1,3)
--    (2,1) (2,2) (2,3) (2,4)
-- (3,1) (3,2) (3,3) (3,4) (3,5)
--    (4,2) (4,3) (4,4) (4,5)
--       (5,3) (5,4) (5,5)

-- whites is a sorted list containing the Pos's corresponding to the white
-- pieces. Similarly, blacks is a sorted list of the Pos's of the black
-- pieces. n is the size of the board.
data Board = Board {whites :: [Pos],
                    blacks :: [Pos],
                    n :: Int
                    }
             deriving (Show,Eq)

-- (Row, Column), 1-indexed, with 1 in top left.
type Pos = (Int, Int)

-- A move contains the original position of the piece being moved, the position
-- it's being moved to, and the player who is moving the piece
data Move = Move {source :: Pos,
                  dest :: Pos,
                  mover :: Char
                  }
            deriving (Show,Eq)

--The closest neighbour in a given direction, with (drow, dcol)
type Dir = (Int, Int)

-------------- Crusher ------------------------------------------

-- (note: in order to ensure that there are no namespace conflicts, we've indented all
--  helper functions so that they are within the 'where' of crusher_i5l8)

-- boardStrings is a list of all the previous boards as strings, with the
-- current board at the head of the list.
-- n is the board size (length of one side of the hexagonal board) (PRECONDITION: must
-- be >= 3)
-- depth is the number of moves to look ahead (PRECONDITION: must be >= 1)
-- Return boardStrings with the best move for player added to the head of the list
crusher_i5l8 :: [String] -> Char -> Int -> Int -> [String]
crusher_i5l8 boardStrings player depth size = unparse move:boardStrings
   where
      move = makeMove boards player depth
      boards = map (parse size) boardStrings


      -----------------MiniMax--------------------------------------------------

      -- INVARIANT:
      --    - WHITE is the MAX player,
      --    - BLACK is the MIN player.
      -- I.e. positive scores indicate the board is better for WHITE, negative scores
      -- indicate the board is better for BLACK.

      -- Produce the board that we determine to be the best move from minimax
      -- If the current board is already in a winning or losing state, return the
      -- current board.
      makeMove :: [Board] -> Char -> Int -> Board
      makeMove boards player depth
         | gameOver  = head boards
         | otherwise = head (fst $ minOrMaxBy (comparing snd) scoredBoards)
         where

            -- Game Over conditions: no valid moves for current player
            -- Or, either player has < n pieces (n is board size)
            gameOver = (null nexts) || (tooFewWhites $ head boards) || (tooFewBlacks $ head boards)
            nexts = map (\b -> b:boards) $ getNextBoardsForPlayer boards player
            scores = map (\b -> (minimax b (other player) (depth-1))) nexts
            scoredBoards = zip nexts scores
            minOrMaxBy = if player == 'W' then maximumBy else minimumBy

      -- Using minimax algorithm starting with @player, produce score of best board
      -- to be generated at @depth. Returns static evaluation if head board is in win-state
      minimax :: [Board] -> Char -> Int -> Int
      minimax (board:history) player depth
         | depth == 0 = evaluate board
         | whiteWon   = winScore board
         | blackWon   = -winScore board
         | otherwise  = minOrMax player scores
         where
            nexts = getNextBoardsForPlayer (board:history) player

            -- Winning conditions: other player has either no legal moves, or < n pieces (where n is board size)
            whiteWon = ((player == 'B') && (null nexts)) || tooFewBlacks board
            blackWon = ((player == 'W') && (null nexts)) || tooFewWhites board

            -- The scores of all the boards reachable in one move from this board
            scores :: [Int]
            scores = map getNextMinimax possibilites
               where
                  getNextMinimax possibility = minimax possibility (other player) (depth-1)
                  possibilites = map (\b -> b:board:history) nexts

      -- Produce the max if W, and the min if B (see MiniMax INVARIANT, above)
      minOrMax :: Char -> [Int] -> Int
      minOrMax 'W' = maximum
      minOrMax 'B' = minimum

      -- Switch player
      other :: Char -> Char
      other 'W' = 'B'
      other 'B' = 'W'



      ---------------- Static Board Evaluation ----------------------------------------

      -- Produce a static score for given board. Uses winScore (which uses board's size) to
      -- ensure generally that winning board's score exceeds difference in piece-counts
      evaluate :: Board -> Int
      evaluate board
          | tooFewBlacks board = winScore board -- white has won
          | tooFewWhites board = -winScore board -- black has won
          | otherwise = whiteCount - blackCount
          where
            whiteCount  = length $ whites board
            blackCount  = length $ blacks board

      tooFewBlacks :: Board -> Bool
      tooFewBlacks board = blackCount < size
         where
            size = n board
            blackCount  = length $ blacks board

      tooFewWhites :: Board -> Bool
      tooFewWhites board = whiteCount < size
         where
            size = n board
            whiteCount  = length $ whites board

      -- Produce the correctly-weighted score for a win (or lose) in given board.
      winScore :: Board -> Int
      winScore board = (n board) * 3

      ----------------- Generating New Boards ---------------------------------

      -- For given history of boards, produce all legal boards 1 move away for given player
      getNextBoardsForPlayer :: [Board] -> Char -> [Board]
      getNextBoardsForPlayer (latest:history) player =
         filter isNew $ getPotentialNextBoards latest player
         where
            isNew = flip notElem history

      -- Produce all possible boards 1 move away for the given player. Some may not be the
      -- result of legal moves as the board may be in the list of previous boards
      getPotentialNextBoards :: Board -> Char-> [Board]
      getPotentialNextBoards board player =
         map (makeMovedBoard board) (getPlayerMoves board player)

      -- Gets all possible moves the given player could make on the given board without
      -- considering previous boards
      getPlayerMoves :: Board -> Char -> [Move]
      getPlayerMoves board player = concatMap (getPieceMoves board player) pieces
         where pieces = fst $ selectPieces board player

      -- Produce (player's pieces, opponent's pieces)
      selectPieces :: Board -> Char -> ([Pos], [Pos])
      selectPieces board 'W' = (whites board, blacks board)
      selectPieces board 'B' = (blacks board, whites board)

      -- Produce all moves the player can make with the piece at @pos on the board
      -- A piece can move to an adjacent location if that position is empty, or
      -- a piece can leap over one adjacent piece of the same type if the landing
      -- position does not contain a piece of the same type.
      getPieceMoves :: Board -> Char -> Pos -> [Move]
      getPieceMoves board player pos =
        concatMap (getMovesInDir board player pos) dirs
        where
          -- enumerating Dirs:
          nw = (-1, -1); ne = (-1, 0)
          w =  ( 0, -1); e  = ( 0, 1)
          sw = ( 1,  0); se = ( 1, 1)
          dirs = [nw,ne,e,se,sw,w] -- the directions in clockwise order

      -- Produce all moves of the piece at pos in given direction
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
      -- NOTE: this pos might NOT be a valid board position
      getNeighbour :: Pos -> Dir -> Pos
      getNeighbour (r,c) (dr, dc) = (r+dr, c+dc)

      -- Return the board produced by making the given move on the given board
      makeMovedBoard :: Board -> Move -> Board
      makeMovedBoard board move
         | player == 'W' = Board {whites = newPlayerPieces, blacks = newOpponentPieces, n = (n board)}
         | otherwise     = Board {whites = newOpponentPieces, blacks = newPlayerPieces, n = (n board)}
         where
            player = mover move
            (playerPieces, opponentPieces) = selectPieces board player
            newPlayerPieces = insert (dest move) $ delete (source move) playerPieces
            newOpponentPieces = delete (dest move) opponentPieces


      ---------------- Parsing ---------------------------------------------------

      -- Convert the input string to our board representation
      parse :: Int -> String -> Board
      parse n string = Board { whites = filterPlayer 'W' positions,
                               blacks = filterPlayer 'B' positions,
                               n = n
                               }
         where positions = getAllPositions string n

      -- Produce all the Pos's in positions that contain player's pieces
      filterPlayer :: Char -> [(Pos, Char)] -> [Pos]
      filterPlayer player positions = map fst $ filter is_player positions
         where is_player (pos,char) = char == player

      -- Produce tuples containing the position on the board and the character at that
      -- position for all characters string
      getAllPositions :: String -> Int -> [(Pos,Char)]
      getAllPositions string n =
         (getTopPositions string n) ++ (getBottomPositions string n)

      -- Return a list of tuples containing the position on the board and the character at that
      -- position for the first n rows of the board (where n is size of board)
      getTopPositions :: String -> Int -> [(Pos,Char)]
      getTopPositions string n = getTopPositions' string 1 n

      -- Return (pos, char at pos) tuples in sorted order for first n rows of board(sorted by Pos)
      getTopPositions' :: String -> Int -> Int -> [(Pos, Char)]
      getTopPositions' string row n
         | row > n = []
         | otherwise = positions ++ (getTopPositions' rest_string (row+1) n)
         where
            row_indices = repeat row
            row_length = n + row - 1
            col_indices = take row_length [1..]

            pos_list = zip row_indices col_indices
            positions = zip pos_list string

            rest_string = drop row_length string

      -- Like getTopPositions, but for last n-1 rows of the board
      getBottomPositions :: String -> Int -> [(Pos,Char)]
      getBottomPositions string n =
         getBottomPositions' (drop top_length string) (n+1) n
         where
            top_length = sum [n..(2*n-1)]

      -- PRECONDITION: string is the original board string minus the characters belonging to the first n
      -- rows;
      -- Return sorted (pos, character at pos) tuples for bottom (less-than-) half of the board
      getBottomPositions' :: String -> Int -> Int -> [(Pos, Char)]
      getBottomPositions' string row n
         | row > 2*n-1 = []
         | otherwise = positions ++ (getBottomPositions' rest_string (row+1) n)
         where
            row_indices = repeat row
            row_length = n + (2*n-1) - row
            col_start = row-n+1
            col_indices = take row_length [col_start..]

            pos_list = zip row_indices col_indices
            positions = zip pos_list string

            rest_string = drop row_length string

      -- Convert our representation of a board to output string representation
      unparse :: Board -> String
      unparse board = map (charAtPosOn board) (getValidPos board)

      -- Produce the character representation of the piece at location pos on board
      charAtPosOn :: Board -> Pos -> Char
      charAtPosOn board pos
         | elem pos (whites board)   = 'W'
         | elem pos (blacks board)   = 'B'
         | otherwise                 = '-'

      -- Produce all valid Pos in given board in the same order that they appear in an input string
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


------------------------------------------------------------
-- Switch player
other :: Char -> Char
other 'W' = 'B'
other 'B' = 'W'


-- Convert the input string to our board representation
parse :: Int -> String -> Board
parse n string = Board { whites = filterPlayer 'W' positions,
                         blacks = filterPlayer 'B' positions,
                         n = n
                         }
   where positions = getAllPositions string n

-- Produce all the Pos's in positions that contain player's pieces
filterPlayer :: Char -> [(Pos, Char)] -> [Pos]
filterPlayer player positions = map fst $ filter is_player positions
   where is_player (pos,char) = char == player

-- Produce tuples containing the position on the board and the character at that
-- position for all characters string
getAllPositions :: String -> Int -> [(Pos,Char)]
getAllPositions string n =
   (getTopPositions string n) ++ (getBottomPositions string n)

-- Return a list of tuples containing the position on the board and the character at that
-- position for the first n rows of the board
getTopPositions :: String -> Int -> [(Pos,Char)]
getTopPositions string n = getTopPositions' string 1 n

-- Return a list of tuple containing the position on the board and the character at that
-- position for the last n-1 rows of the board
getBottomPositions :: String -> Int -> [(Pos,Char)]
getBottomPositions string n =
   getBottomPositions' (drop top_length string) (n+1) n
   where
      top_length = sum [n..(2*n-1)]

-- Return positions in sorted order (sorted by Pos)
getTopPositions' :: String -> Int -> Int -> [(Pos, Char)]
getTopPositions' string row n
   | row > n = []
   | otherwise = positions ++ (getTopPositions' rest_string (row+1) n)
   where
      row_indices = repeat row
      row_length = n + row - 1
      col_indices = take row_length [1..]

      pos_list = zip row_indices col_indices
      positions = zip pos_list string

      rest_string = drop row_length string

-- string is the original board string minus the characters belonging to the first n
-- rows, i.e. the original string minus the first sum [n..(2*n-1)] characters.
-- Return positions in sorted order (sorted by Pos)
getBottomPositions' :: String -> Int -> Int -> [(Pos, Char)]
getBottomPositions' string row n
   | row > 2*n-1 = []
   | otherwise = positions ++ (getBottomPositions' rest_string (row+1) n)
   where
      row_indices = repeat row
      row_length = n + (2*n-1) - row
      col_start = row-n+1
      col_indices = take row_length [col_start..]

      pos_list = zip row_indices col_indices
      positions = zip pos_list string

      rest_string = drop row_length string


-- Convert our representation of a board to output string representation
unparse :: Board -> String
unparse board = map (charAtPosOn board) (getValidPos board)

-- Produce the character representation of the piece at location pos on board
charAtPosOn :: Board -> Pos -> Char
charAtPosOn board pos
   | elem pos (whites board)   = 'W'
   | elem pos (blacks board)   = 'B'
   | otherwise                 = '-'

-- Produce all valid Pos in given board in the same order that they appear
-- in an input string
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

play :: Int -> Int -> Char -> [String]
play turns d p = play' turns [string3] p d

play' :: Int -> [String] -> Char -> Int -> [String]
play' 0 history _ _ = history
play' turns history p d =
   play' (turns-1) (crusher_i5l8 history p d 3) (other p) d    

playIt turns d p = reverse (map splitBoard (play turns d p))

hasDuplicates turns d p = length (nub result) /= length result
   where
      result = play turns d p

showGame turns d p = printStrMatrix $ playIt turns d p

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
board2 = Board {whites = [pos1,pos2], blacks = [], n = 3}
string3 = "WWW-WW-------BB-BBB"
board3 = parse 3 string3
string4 = "WWWW-WWW---WW-----------BB---BBB-BBBB"  -- just guessing ...
board4 = parse 4 string4

