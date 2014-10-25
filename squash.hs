-- board_string is the current board as a string, history_strings is a list of
-- all the previous boards as strings
crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (board_string:history_strings) player depth n =
   unparse (best_next_move board player depth history):board_string:history_strings
   where board = parse n board_string
         history = map (parse n) history_strings

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

getTopPositions string n = getTopPositions' string 1 n
getBottomPositions string n =
   getBottomPositions' (drop top_length string) (n+1) n
   where top_length = sum [n..(2*n-1)]


-- for the first n rows
getTopPositions' :: String -> Int -> Int -> [(Pos, Char)]
getTopPositions' string row n
   | row > n = []
   | otherwise = positions ++ (getTopPositions' rest_string (row+1) n)
   where row_indices = cycle [row]
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
   where row_indices = cycle [row]
         row_length = n + (2*n-1) - row
         col_start = row-n+1
         col_indices = take row_length [col_start..]

         pos_list = zip row_indices col_indices
         positions = zip pos_list string

         rest_string = drop row_length string





-- converts our representation to output string
unparse :: Board -> String
unparse board = "" -- TODO

best_next_move :: Board -> Char -> Int -> [Board] -> Board
best_next_move board player depth history = board1 -- TODO






-------------- data types -----------------------------------------

data Board = Board {whites :: [Pos],
                    blacks :: [Pos],
                    n :: Int
                    }
             deriving (Show,Eq)

type Pos = (Int, Int)

data Pos2 = Pos2 {row :: Int,
                  col :: Int
                  }
            deriving (Show,Eq)

pos1 = (1,1)
pos2 = (2,2)
pos2_1 = Pos2 {row = 1, col = 1}
pos2_2 = Pos2 {row = 2, col = 2}



board1 = Board {whites = [pos1,pos2], blacks = [], n = 3}
board_string = "WWW-WW-------BB-BBB"