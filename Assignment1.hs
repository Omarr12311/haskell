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
      -- Reconstruct the current row
      currentRow = reverse left ++ [focus] ++ right
      
      -- Reconstruct the full grid
      fullGrid = above ++ [currentRow] ++ below
      
      -- Position of focus
      focusRow = length above
      focusCol = length left
      
      -- Convert to strings with highlighting
      strGrid = [[if rowIdx == focusRow && colIdx == focusCol
                  then "\ESC[44m" ++ show elem ++ "\ESC[0m"
                  else show elem
                  | (colIdx, elem) <- zip [0..] row]
                 | (rowIdx, row) <- zip [0..] fullGrid]
      
      -- Calculate column widths
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      
      -- Format rows
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
    newAboveRows = init aboveRows  -- all rows except the last
    targetRow = last aboveRows     -- the row we're moving to
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
    gwp1 = moveUp gwp          -- Move up
    gwp2 = put n gwp1          -- Put n at position above
    gwp3 = moveDown gwp2       -- Move back to current
    gwp4 = put n gwp3          -- Put n at current position
  in gwp4

  
putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown n gwp = 
  let 
    gwp1 = put n gwp           -- Put n at current position
    gwp2 = moveDown gwp1       -- Move down
    gwp3 = put n gwp2          -- Put n at position below
    gwp4 = moveUp gwp3         -- Move back to original position
  in gwp4

putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight n gwp = 
  let 
    gwp1 = put n gwp           -- Put n at current position
    gwp2 = moveRight gwp1      -- Move right
    gwp3 = put n gwp2          -- Put n at right position
    gwp4 = moveLeft gwp3       -- Move back to original position
  in gwp4

  
putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft n gwp = 
  let 
    gwp1 = moveLeft gwp        -- Move left
    gwp2 = put n gwp1          -- Put n at left position
    gwp3 = moveRight gwp2      -- Move back to current
    gwp4 = put n gwp3          -- Put n at current position
  in gwp4


---------------------------------------------------------------------------------
-- TASK 4
---------------------------------------------------------------------------------

cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover = undefined
