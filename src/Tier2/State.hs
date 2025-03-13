module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = do
    s <- get
    let newAcc = ax s + bx s
    put $ s { acc = newAcc, blink = False }
    return newAcc

minus :: Calculation
minus = do
    s <- get
    let newAcc = ax s - bx s
    put $ s { acc = newAcc, blink = False }
    return newAcc

productS :: Calculation
productS = do
    s <- get
    let newAcc = ax s * bx s
    put $ s { acc = newAcc, blink = False }
    return newAcc

div :: Calculation
div = do
    s <- get
    if bx s == 0
        then put emptyRegisters
        else do
            let newAcc = ax s `div` bx s
            put $ s { acc = newAcc, blink = False }
    s' <- get
    return $ acc s'

swap :: Calculation
swap = do
    s <- get
    put $ s { ax = bx s, bx = ax s }
    return 0

blinkS :: Calculation
blinkS = do
    s <- get
    put $ s { blink = not (blink s) }
    return 0

accS :: Calculation
accS = do
    s <- get
    let currentBlink = blink s
        newA = if currentBlink then ax s else acc s
        newB = if currentBlink then acc s else bx s
    put $ s { ax = newA, bx = newB, blink = not currentBlink }
    return (acc s)

number :: Int -> Calculation
number x = do
    s <- get
    let currentBlink = blink s
        newA = if currentBlink then ax s else x
        newB = if currentBlink then x else bx s
    put $ s { ax = newA, bx = newB, blink = not currentBlink }
    return x

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "/" -> div
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters