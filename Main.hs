import System.IO

data Instruction = Addx Int | Noop deriving Show

main = do
  contents <- readFile "Example.txt"
  let instructions = map toInstr $ lines $ contents
  print instructions

toInstr :: String -> Instruction
toInstr "noop" = Noop
toInstr add = Addx $ read $ head $ tail $ words add