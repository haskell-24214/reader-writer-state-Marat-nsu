module Tier0.Writer (Tree (..), sumAndTraceInOrder) where

import Control.Monad.Writer

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

sumAndTraceInOrder :: Num a => Tree a -> Writer [a] a
sumAndTraceInOrder (Leaf x) = do
    tell [x]
    return x
sumAndTraceInOrder (Branch l x r) = do
    lSum <- sumAndTraceInOrder l
    tell [x]
    rSum <- sumAndTraceInOrder r
    return $ lSum + x + rSum