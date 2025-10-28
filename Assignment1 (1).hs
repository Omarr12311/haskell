-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment1 ( put,
                     moveLeft,
                     moveRight,
                     moveUp,
                     moveDown,
                     Grid(..),
                     GridWithAPointer(..),
                     putTatamiDown,
                     putTatamiUp,
                     putTatamiLeft,
                     putTatamiRight,
                     cover
             ) where

import Data.Char (isLetter)

-- these two function are to correctly measure the width of an entry of a grid, 
-- i.e. so that the width of "\ESC[44m55\ESC[0m" ignored the escape sequences
stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':xs) = stripANSI (drop 1 (dropWhile (not . isLetter) xs))
stripANSI (x:xs) = x : stripANSI xs

visibleLength :: String -> Int
visibleLength = length . stripANSI

newtype Grid a = Grid { grid :: [[a]] } deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid g)
    | null g = ""
    | otherwise = unlines (map showRow g)
    where
      strGrid = map (map show) g
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths (map show row)]
      padRight n s = s ++ replicate (n - visibleLength s) ' '

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


newtype GridWithAPointer a = GridWithAPointer (Grid a, [a], a, [a], Grid a)
  deriving Eq


---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- TASK 1
---------------------------------------------------------------------------------

instance (Show a) => Show (GridWithAPointer a) where
  show (GridWithAPointer (Grid above, left, focus, right, Grid below)) = 
    let
      currentRow = reverse left ++ [focus] ++ right
      fullGrid = above ++ [currentRow] ++ below
      focusRow = length above
      focusCol = length left
      strGrid = [[if rowIdx == focusRow && colIdx == focusCol
                  then "\ESC[44m" ++ show elem ++ "\ESC[0m"
                  else show elem
                  | (colIdx, elem) <- zip [0..] row]
                 | (rowIdx, row) <- zip [0..] fullGrid]
      
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths row]
      padRight n s = s ++ replicate (n - visibleLength s) ' '
      
    in if null fullGrid
       then ""
       else unlines (map showRow strGrid)
---------------------------------------------------------------------------------
-- TASK 2
---------------------------------------------------------------------------------

put :: a -> GridWithAPointer a -> GridWithAPointer a
put newVal (GridWithAPointer (above, left, _, right, below)) = 
  GridWithAPointer (above, left, newVal, right, below)

moveLeft :: GridWithAPointer a -> GridWithAPointer a
moveLeft (GridWithAPointer (above, [], _, _, _)) = 
  error "Cannot move left: already at leftmost position"
moveLeft (GridWithAPointer (above, l:ls, focus, right, below)) = 
  GridWithAPointer (above, ls, l, focus:right, below)


moveRight :: GridWithAPointer a -> GridWithAPointer a
moveRight (GridWithAPointer (above, _, _, [], _)) = 
  error "Cannot move right: already at rightmost position"
moveRight (GridWithAPointer (above, left, focus, r:rs, below)) = 
  GridWithAPointer (above, focus:left, r, rs, below)

moveUp :: GridWithAPointer a -> GridWithAPointer a
moveUp (GridWithAPointer (Grid [], _, _, _, _)) = 
  error "Cannot move up: already at top row"
moveUp (GridWithAPointer (Grid aboveRows, left, focus, right, Grid belowRows)) = 
  let 
    currentRow = reverse left ++ [focus] ++ right
    colIndex = length left
    newAboveRows = init aboveRows  
    targetRow = last aboveRows     
    (newLeft, newFocus:newRight) = splitAt colIndex targetRow
  in GridWithAPointer (Grid newAboveRows, reverse newLeft, newFocus, newRight, Grid (currentRow:belowRows))

moveDown :: GridWithAPointer a -> GridWithAPointer a
moveDown (GridWithAPointer (_, _, _, _, Grid [])) = 
  error "Cannot move down: already at bottom row"
moveDown (GridWithAPointer (Grid aboveRows, left, focus, right, Grid (row:rows))) = 
  let 
    currentRow = reverse left ++ [focus] ++ right
    colIndex = length left
    (newLeft, newFocus:newRight) = splitAt colIndex row
  in GridWithAPointer (Grid (aboveRows ++ [currentRow]), reverse newLeft, newFocus, newRight, Grid rows)

---------------------------------------------------------------------------------
-- TASK 3
---------------------------------------------------------------------------------

putTatamiUp :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiUp n gwp = 
  let 
    gwp1 = moveUp gwp
    gwp2 = put n gwp1          
    gwp3 = moveDown gwp2       
    gwp4 = put n gwp3          
  in gwp4

  
putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown n gwp = 
  let 
    gwp1 = put n gwp          
    gwp2 = moveDown gwp1      
    gwp3 = put n gwp2          
    gwp4 = moveUp gwp3         
  in gwp4

putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight n gwp = 
  let 
    gwp1 = put n gwp           
    gwp2 = moveRight gwp1      
    gwp3 = put n gwp2          
    gwp4 = moveLeft gwp3       
  in gwp4

  
putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft n gwp = 
  let 
    gwp1 = moveLeft gwp        
    gwp2 = put n gwp1          
    gwp3 = moveRight gwp2      
    gwp4 = put n gwp3          
  in gwp4


---------------------------------------------------------------------------------
-- TASK 4
---------------------------------------------------------------------------------
cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover (GridWithAPointer (Grid above, left, focus, right, Grid below)) =
  let
    h = length above + 1 + length below
    w = length left  + 1 + length right
    buildRowPairs :: Int -> Integer -> ([Integer], Integer)
    buildRowPairs 0 k = ([], k)
    buildRowPairs n k
      | n >= 2 =
          let (rest, k2) = buildRowPairs (n - 2) (k + 1)
          in (k : k : rest, k2)
      | otherwise = error "cover: expected even number of columns"

    buildAllRowsHoriz :: Int -> Int -> Integer -> ([[Integer]], Integer)
    buildAllRowsHoriz 0 _ k = ([], k)
    buildAllRowsHoriz r w k =
      let (row, k1)   = buildRowPairs w k
          (rows, k2)  = buildAllRowsHoriz (r - 1) w k1
      in (row : rows, k2)

    
    twoRowsVert :: Int -> Integer -> ([Integer], [Integer], Integer)
    twoRowsVert w k =
      let row  = take w [k ..]
      in (row, row, k + fromIntegral w)

    buildAllRowsVert :: Int -> Int -> Integer -> ([[Integer]], Integer)
    buildAllRowsVert 0 _ k = ([], k)
    buildAllRowsVert r w k
      | r >= 2 =
          let (r1, r2, k1) = twoRowsVert w k
              (rest, k2)   = buildAllRowsVert (r - 2) w k1
          in (r1 : r2 : rest, k2)
      | otherwise = error "cover: expected even number of rows"

    tiled :: [[Integer]]
    tiled
      | even w =
          let (g, _) = buildAllRowsHoriz h w 1
          in g
      | even h =
          let (g, _) = buildAllRowsVert  h w 1
          in g
      | otherwise = error "cover: cannot tile a grid with both odd dimensions"

    topRow    = head tiled
    topCell   = head topRow
    restRow   = tail topRow
    restRows  = tail tiled
  in GridWithAPointer (Grid [], [], topCell, restRow, Grid restRows)
