module Sudoku where

import Test.QuickCheck
import Data.Char
import System.Random
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 =
    Sudoku
      [ [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      , [j 3,j 6,j 3,j 3,j 4,j 5,j 6,j 7,j 9]
      ]
  where
        n = Nothing
        j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku[[Nothing|x<-[1..9]]| x<-[1..9] ] 

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s)==9 && and[length c==9 | c <- rows s ] && controlCells
  where controlCells = and[and[(c>=Just 1 && c <= Just 9)||c == Nothing | c <- cs] | cs <- rows s]


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = and[and[not(c == Nothing) | c <- cs] | cs <- rows s]

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku []) = return ()
printSudoku (Sudoku (r:rs)) = 
    do
        putStrLn $ printRow r
        printSudoku (Sudoku rs)

printRow :: Row -> String
printRow [] = ""
printRow (Just c:cs) = show c ++ "  " ++ printRow cs 
printRow (Nothing :cs) = "." ++ "  " ++ printRow cs



-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f = 
    do
        s <- readFile f
        let sudd = [[if isDigit c then Just (digitToInt c) else Nothing | c <- r]| r <- take 9 (lines s) ] 
        return (Sudoku sudd)

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency[(1, nums), (3, noNum)]
     where
        nums = elements [Just n | n <- [1..9]]
        noNum = elements[Nothing]

-- * C2
-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where 
     arbitrary = do
                    r <- vectorOf 9 $ vectorOf 9 cell
                    return (Sudoku r)
-- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock ((Just c):cs) | length cs > 0 = not(Just c `elem` cs) && isOkayBlock cs
                          | otherwise = not(Just c `elem` cs)
isOkayBlock ((Nothing):cs)| length cs > 0 = isOkayBlock cs
                          | otherwise = True




-- * D2

blocks :: Sudoku -> [Block]
blocks s = rowBlocks s' ++ columnBlocks s' ++ squareBlockOne s'
  ++ squareBlockTwo s' ++ squareBlockThree s'
    where s' = rows s

rowBlocks :: [Row] -> [Block]
rowBlocks r = r

columnBlocks :: [Row] -> [Block]
columnBlocks c = transpose c

squareBlockOne :: [[Maybe Int]] -> [[Maybe Int]]
squareBlockOne [] = []
squareBlockOne (x1:x2:x3:xs) = ((take 3 x1) ++ (take 3 x2)
                               ++ (take 3 x3)) : squareBlockOne xs

squareBlockTwo :: [[Maybe Int]] -> [[Maybe Int]]
squareBlockTwo [] = []
squareBlockTwo (x1:x2:x3:xs) = ((take 3 (drop 3 x1)) ++ (take 3 (drop 3 x2))
                               ++ (take 3 (drop 3 x3))) : squareBlockTwo xs

squareBlockThree :: [[Maybe Int]] -> [[Maybe Int]]
squareBlockThree [] = []
squareBlockThree (x1:x2:x3:xs) = ((take 3 (drop 6 x1)) ++ (take 3 (drop 6 x2))
                                 ++ (take 3 (drop 6 x3))) : squareBlockThree xs

-- Props for blocks (D2)
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length (blocks s) == 27


-- * D3

isOkay :: Sudoku -> Bool
isOkay s = and[isOkayBlock b | b <- blocks s] 


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks s = [(a,b) | a <- [0..8], b <- [0..8], posIsBlank (a,b) s]

posIsBlank :: Pos -> Sudoku -> Bool
posIsBlank (x,y) (Sudoku s) = (s !! x) !! y == Nothing 

prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks s = and[((rows s)!!x!!y) == Nothing | (x,y) <- (blanks s)]


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (i,y) = []
xs !!= (i,y) | i < 0 = xs 
             | i > length xs - 1 = xs
             | otherwise = take i xs ++ [y] ++ drop (i+1) xs

--Släng gärna ett till getöga på detta änna
prop_bangBangEquals_correct :: [Int] -> Int -> Int -> Bool
prop_bangBangEquals_correct x i value = length x == length (x !!= (i,value))

-- * E3

updateRow :: [[Maybe Int]] -> Int -> Int -> Cell -> [[Maybe Int]]
updateRow [] _ _ _= []
updateRow x i i2 _ | i < 0 || i2<0 = x
updateRow (x:xs) 0 i2 c = (x !!= (i2,c)) : xs
updateRow (x:xs) i i2 c = x : updateRow xs (i-1) i2 c

update :: Sudoku -> Pos -> Cell -> Sudoku
update s (x,y) c = Sudoku (updateRow r x y c)
  where r = rows s

--prop_update_updated =

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated s (x,y) c
  | s == Sudoku [] || x<0 || y<0 || x>8 || y>8 = update s (x,y) c == s
  | otherwise = ((rows s2)!!x!!y) == c
    where s2 = update s (x,y) c
------------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve s | s' == [] = Nothing
        | otherwise = (head(s'))
          where s' = solve2 s

solve2 :: Sudoku -> [Maybe Sudoku]
solve2 s | not(isSudoku s) = []
solve2 s | not(isOkay s) = []
solve2 s | isFilled s = [Just s]
solve2 s = solve3 s'
  where s' = [update s pos (Just c) | c <- [1..9]]
        pos = head (blanks s)

solve3 :: [Sudoku] -> [Maybe Sudoku]
solve3 [] = []
solve3 (x:xs) = solve2 x ++ solve3 xs
-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve f = do
                   s <- readFile f
                   let sud = Sudoku [[if isDigit c then Just (digitToInt c) else Nothing | c <- r]| r <- take 9 (lines s) ]
                   let solved = solve sud
                   if solved == Nothing
                     then putStrLn "(no solution)"
                     else printSudoku $ fromJust (solved)
-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol s = and[ if cellAtPos s (x, y) == Nothing then True
                        else if cellAtPos s (x, y) == cellAtPos sol (x, y) then True 
                        else False |x <- [0..8], y <- [0..8] ] 
                        && isFilled sol
                        && isOkay sol
                        && isOkay s

cellAtPos :: Sudoku -> Pos -> Cell
cellAtPos (Sudoku r) (x,y) = (r!!x) !! y 

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s | s' == Nothing = collect(1) $ property True 
  where s' = solve s
prop_SolveSound s =  isOkay s ==> isSolutionOf (fromJust (solve s)) s

fewerChecks prop =
  quickCheckWith stdArgs{maxSuccess=30 } prop
