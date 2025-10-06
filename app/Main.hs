module Main (main) where

import RBBag (RBBag, add, empty, logTree, mapTree)

bag1 :: RBBag.RBBag Integer
bag1 = add 3 $ add (-1) $ add (-2) empty

main :: IO ()
main = do
  logTree bag1
  logTree $ mapTree abs bag1
