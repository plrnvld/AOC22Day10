import System.IO

data Instruction = Addx Int | Noop deriving (Show)

main = do
  contents <- readFile "Example.txt"
  let instructions = map toInstr $ lines $ contents
      withCycles = map (\i -> (i, cycleTime i, xChange i)) instructions
      withTotals = scanl1 (\(i1, c1, x1) (i2, c2, x2) -> (i2, c1 + c2, x1 + x2)) withCycles
      withSignal = map (\(i, c, x) -> (i, c, x, c * x)) withTotals
  print withSignal

toInstr :: String -> Instruction
toInstr "noop" = Noop
toInstr add = Addx $ read $ head $ tail $ words add

cycleTime :: Instruction -> Int
cycleTime Noop = 1
cycleTime (Addx _) = 2

xChange :: Instruction -> Int
xChange Noop = 0
xChange (Addx change) = change