module Main (main) where

import RBBag

bag1 = add 3 $ add 1 $ add 2 empty
bag2 = add 3 $ add 1 $ add 2 empty

main :: IO ()
main = do
    logTree bag1
    logTree bag2
    print $ bag1 ?= bag2