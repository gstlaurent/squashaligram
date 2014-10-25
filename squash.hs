-- board_string is the current board as a string, history_strings is a list of
-- all the previous boards as strings
crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (board_string:history_strings) player depth size = 
   unparse (best_next_move board player depth history):board_string:history_strings
   where board = parse size board_string
         history = map (parse size) history_strings

-- converts input string to our board representation
parse :: Int -> String -> Board
parse size string = Board { whites = filterPlayer 'W' positions,
                            blacks = filterPlayer 'B' positions,
                            size = size
                            }
   where positions = getAllPositions string size

filterPlayer :: Char -> [(Pos, Char)] -> [Pos]
filterPlayer player positions = map fst $ filter is_player positions
   where is_player (pos,char) = char == player

getAllPositions string size =
   (getTopPositions string size) ++ (getBottomPositions string size)

getTopPositions string size = concat getTopPositions' string 1 size
getBottomPositions string size =
   concat getBottomPositions' (drop top_length string) (size+1) size
   where top_length = sum [size..(2*size-1)] -- sum of the first size rows
                                      

-- for the first n rows    
getTopPositions' string row_index n
   | row_index > n = []
   | otherwise = this_rows_positions:(getTopPositions' next_string (row_index+1) n)
   where row_list = row_index:row_list
         row_length = n + row_index - 1       
         col_list = [1..row_length]
         positions = zip row_list col_list
         current_row_string = take row_length string
         next_string = drop row_length string
         this_rows_positions = zip (zip row_list col_list) current_row_string
         

-- for the rest of the rows (the next n-1 rows)
-- must pass in the string with the first n rows removed
-- this is the original string minus the first n^2 + n^2/2 - n characters
getBottomPositions' string row_index n
   | row_index > 2*n-1 = []
   | otherwise = this_rows_positions:(getBottomPositions' next_string (row_index+1) n)
   where row_list = row_index:row_list
         row_length = n + (2*n-1-row_index)
         col_start = row_index-n+1
         col_list = [col_start..(row_length+col_start-1)]
         positions = zip row_list col_list
         current_row_string = take row_length string
         next_string = drop row_length string
         this_rows_positions = zip (zip row_list col_list) current_row_string

    


-- converts our representation to output string
unparse :: Board -> String
unparse board = "" -- TODO

best_next_move :: Board -> Char -> Int -> [Board] -> Board
best_next_move board player depth history = board1 -- TODO






-------------- data types -----------------------------------------

data Board = Board {whites :: [Pos],
                    blacks :: [Pos],
                    size :: Int
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



board1 = Board {whites = [pos1,pos2], blacks = [], size = 3}
