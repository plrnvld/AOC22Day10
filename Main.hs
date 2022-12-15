{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Debug.Trace

data Instruction = Addx Int | Noop deriving (Show)

main = do
  contents <- readFile "Input.txt"
  let instructions = map toInstr $ lines contents
      withCycles = map (\i -> (i, cycleTime i, xChange i)) instructions
      withTotals = scanl (\(i1, c1, x1) (i2, c2, x2) -> (i2, c1 + c2, x1 + x2)) (Noop, 0, 1) withCycles
      withSignal = map (\(i, c, x) -> (i, c, x, closestTo20Mod40 c * x)) withTotals
      on20mod40 = keepWhen 
          (\(i, c, x, s) -> c `mod` 40 < 20) 
          (\(i, c, x, s) -> c `mod` 40 == 20 || c `mod` 40 == 21)
          withSignal
      signals = map (\(i, c, x, s) -> s) on20mod40
      total = sum signals
  print total

toInstr :: String -> Instruction
toInstr "noop" = Noop
toInstr add = Addx $ read $ head $ tail $ words add

cycleTime :: Instruction -> Int
cycleTime Noop = 1
cycleTime (Addx _) = 2

xChange :: Instruction -> Int
xChange Noop = 0
xChange (Addx change) = change

keepWhen :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
keepWhen p q [] = []
keepWhen p q [x] = []
keepWhen p q (x1:x2:xs) = if p x1 && q x2 then x1 : keepWhen p q (x2:xs) else keepWhen p q (x2:xs)

closestTo20Mod40 :: Int -> Int
closestTo20Mod40 n = let res = n `mod` 40
                         diff = 20 - res
                         in n + diff