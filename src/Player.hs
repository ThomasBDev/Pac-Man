module Player where

lives :: Int
lives = 3

ghostCombo :: Int
ghostCombo = 0

score :: Int
score = 0

eatPacDot :: Int -> Int
eatPacDot s = s + 100