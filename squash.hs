import Data.List
import Data.Ord

-------------- Data Types and Constants -----------------------------------
{-
We've assigned each character in the string to a position on the board.

For example, a board of size 3 has the following layout:

      (1,1) (1,2) (1,3)
   (2,1) (2,2) (2,3) (2,4)
(3,1) (3,2) (3,3) (3,4) (3,5)
   (4,2) (4,3) (4,4) (4,5)
      (5,3) (5,4) (5,5)

The lists whites and blacks contain Pos's in sorted order.
-}

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

--The closest neighbour in a given direction, with (drow, dcol)
type Dir = (Int, Int)

-------------- Crusher ------------------------------------------
-- (note: in order to ensure that there are no namespace conflicts, we've intended all
--  helper functions so that they are within the 'where' of crusher_i5l8)

-- boardStrings is a list of all the previous boards as strings, with the
-- current board at the head of the list.
-- n is the board size (length of one side of the hexagonal board) (must be >= 3)
-- depth is the number of moves to look ahead (PRECONDITION: must be >= 1)
-- returns boardStrings with the best move for player added to the head of the list
crusher_i5l8 :: [String] -> Char -> Int -> Int -> [String]
crusher_i5l8 boardStrings player depth size = unparse move:boardStrings
   where
      move = makeMove boards player depth
      boards = map (parse size) boardStrings

      -----------------MiniMax--------------------------------------------------
      -- White is max player, black is min player

      -- Produces the board that we determine to be the best move from minimax
      -- If the current board is already in a winning state or losing state, returns the
      -- current board.
      makeMove :: [Board] -> Char -> Int -> Board
      makeMove boards player depth
         | gameOver  = head boards
         | otherwise = head (fst $ minOrMaxBy (comparing snd) scoredBoards)
         where
            -- the game is over if the player has no valid moves, or if either player has < n
            -- pieces, where n is the size of the board
            gameOver = (null nexts) || (tooFewWhites $ head boards) || (tooFewBlacks $ head boards)
            nexts = map (\b -> b:boards) $ getNextBoardsForPlayer boards player
            scores = map (\b -> (minimax b (other player) (depth-1))) nexts
            scoredBoards = zip nexts scores
            minOrMaxBy = if player == 'W' then maximumBy else minimumBy

      -- returns the score of the best evaluation at depth
      minimax :: [Board] -> Char -> Int -> Int
      minimax (board:history) player depth
         | depth == 0 = evaluate board
         | whiteWon   = 3*(n board)
         | blackWon   = -3*(n board)
         | otherwise  = minOrMax player scores
         where
            nexts = getNextBoardsForPlayer (board:history) player
            whiteWon = ((player == 'B') && (null nexts)) || tooFewBlacks board
            blackWon = ((player == 'W') && (null nexts)) || tooFewWhites board

            -- The scores of all the subboards from this board
            scores :: [Int]
            scores = map getNextMinimax possibilites
               where
                  getNextMinimax possibility = minimax possibility (other player) (depth-1)
                  possibilites = map (\b -> b:board:history) nexts

      -- produce the max if W, and the min if B
      minOrMax :: Char -> [Int] -> Int
      minOrMax 'W' = maximum
      minOrMax 'B' = minimum

      -- Switch player
      other :: Char -> Char
      other 'W' = 'B'
      other 'B' = 'W'


      ---------------- Static Board Evaluation ----------------------------------------

      evaluate :: Board -> Int
      evaluate board
          | tooFewBlacks board =  3 * size -- white has won
          | tooFewWhites board = -3 * size -- black has won
          | otherwise = whiteCount - blackCount
          where
            size = n board
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

        where
          -- enumerating Dirs:
          nw = (-1, -1); ne = (-1, 0)
          w =  ( 0, -1); e  = ( 0, 1)
          sw = ( 1,  0); se = ( 1, 1)
          dirs = [nw,ne,e,se,sw,w] -- the directions in clockwise order


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
      -- NOTE: this pos might NOT be a valid board position
      getNeighbour :: Pos -> Dir -> Pos
      getNeighbour (r,c) (dr, dc) = (r+dr, c+dc)

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
      -- converts input string to our board representation
      parse :: Int -> String -> Board
      parse n string = Board { whites = filterPlayer 'W' positions,
                               blacks = filterPlayer 'B' positions,
                               n = n
                               }
         where positions = getAllPositions string n

      filterPlayer :: Char -> [(Pos, Char)] -> [Pos]
      filterPlayer player positions = map fst $ filter is_player positions
         where is_player (pos,char) = char == player

      getAllPositions :: String -> Int -> [(Pos,Char)]
      getAllPositions string n =
         (getTopPositions string n) ++ (getBottomPositions string n)

      -- Returns tuples containing the position on the board and the character at that
      -- position for the first n rows of the board
      getTopPositions :: String -> Int -> [(Pos,Char)]
      getTopPositions string n = getTopPositions' string 1 n

      -- Returns tuples containing the position on the board and the character at that
      -- position for the last n-1 rows of the board
      getBottomPositions :: String -> Int -> [(Pos,Char)]
      getBottomPositions string n =
         getBottomPositions' (drop top_length string) (n+1) n
         where
            top_length = sum [n..(2*n-1)]

      -- returns positions in sorted order (sorted by Pos)
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

      -- string is the original board string with characters belonging to the first n
      -- rows removed. This is the original string minus the first sum [n..(2*n-1)]
      -- characters.
      -- returns positions in sorted order (sorted by Pos)
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