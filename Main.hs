import Debug.Trace
import System.IO

data Instruction = Addx Int | Noop | Prev deriving (Show)

main = do
  contents <- readFile "Input.txt"
  let instructions = map toInstr $ lines contents
      withCycles = map (\i -> (i, cycleTime i, xChange i)) instructions
      withTotals = scanl (\(i1, c1, x1) (i2, c2, x2) -> (i2, c1 + c2, x1 + x2)) (Noop, 0, 1) withCycles
      allCycles = fillCycles withTotals
      withPixels = map (\(i, c, x) -> (i, c, x, if abs (c `mod` 40 - x) <= 1 then '#' else '.')) allCycles
      onlyPixels = map (\(i, c, x, p) -> p) withPixels
      splitted = splitEvery 40 onlyPixels
      joined = concatMap ("\n" ++) splitted
  putStr joined

toInstr :: String -> Instruction
toInstr "noop" = Noop
toInstr add = Addx $ read $ head $ tail $ words add

cycleTime :: Instruction -> Int
cycleTime Noop = 1
cycleTime (Addx _) = 2
cycleTime Prev = error "Prev should not happen here"

xChange :: Instruction -> Int
xChange Noop = 0
xChange (Addx change) = change
xChange Prev = error "Prev should not happen here"

fillCycles :: [(Instruction, Int, Int)] -> [(Instruction, Int, Int)]
fillCycles [] = []
fillCycles [tup] = [tup]
fillCycles (tup1@(i1, c1, x1):tup2@(i2, c2, x2):tups) 
    | c1 + 1 == c2 = tup1: fillCycles (tup2:tups)
    | c1 + 2 == c2 = tup1: (Prev, c1 + 1, x1) : fillCycles (tup2:tups)
    | otherwise = error "No other differences between cycles expected"

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)