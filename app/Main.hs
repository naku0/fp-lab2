module Main (main) where

import RBBag

bag = add 3 $ add 1 $ add 2 empty

main :: IO ()
main = do
    logTree bag