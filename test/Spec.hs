{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid ()
import RBBag
  ( RBBag,
    add,
    contains,
    count,
    delete,
    delete',
    empty,
    filterTree,
    foldlTree,
    foldrTree,
    isEmpty,
    mapTree,
    size,
    union,
    (?/=),
    (?<),
    (?<=),
    (?=),
    (?>),
    (?>=),
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Test.QuickCheck ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Unit tests" $ do
    describe "BAZA operations" $ do
      it "empty tests" $ do
        isEmpty empty `shouldBe` True
        size empty `shouldBe` 0

      it "add tests" $ do
        let bag = add 1 $ add 2 $ add 3 empty
        contains bag 1 `shouldBe` True
        contains bag 2 `shouldBe` True
        contains bag 3 `shouldBe` True
        size bag `shouldBe` 3

      it "add duplicates tests" $ do
        let bag = add 1 $ add 1 empty
        count 1 bag `shouldBe` 2
        size bag `shouldBe` 2

      it "delete tests" $ do
        let bag = add 1 $ add 2 empty
        let bag' = delete 1 bag
        contains bag' 1 `shouldBe` False
        contains bag' 2 `shouldBe` True
        size bag' `shouldBe` 1

      it "delete' tests" $ do
        let bag = add 1 $ add 1 $ add 1 empty
        let bag' = delete' 1 bag
        count 1 bag' `shouldBe` 0
        isEmpty bag' `shouldBe` True

    describe "Query operations" $ do
      it "contains tests" $ do
        let bag = add 5 empty
        contains bag 5 `shouldBe` True
        contains bag 10 `shouldBe` False

      it "count tests" $ do
        let bag = add 1 $ add 1 $ add 2 empty
        count 1 bag `shouldBe` 2
        count 2 bag `shouldBe` 1
        count 3 bag `shouldBe` 0

    describe "Multiset operations" $ do
      it "union combines bags" $ do
        let bag1 = add 1 $ add 2 empty
        let bag2 = add 2 $ add 3 empty
        let united = bag1 `union` bag2
        count 1 united `shouldBe` 1
        count 2 united `shouldBe` 2
        count 3 united `shouldBe` 1
        size united `shouldBe` 4

    describe "Eq operations" $ do
      it "empty bags are equal" $ do
        (empty :: RBBag Int) ?= empty `shouldBe` True
        (empty :: RBBag Int) ?/= empty `shouldBe` False

      it "same elements are equal" $ do
        let bag1 = add 1 $ add 2 empty
        let bag2 = add 2 $ add 1 empty
        bag1 ?= bag2 `shouldBe` True

      it "different elements are not equal" $ do
        let bag1 = add 1 empty
        let bag2 = add 2 empty
        bag1 ?= bag2 `shouldBe` False
        bag1 ?/= bag2 `shouldBe` True

      it "empty < non-empty" $ do
        (empty :: RBBag Int) ?< add 1 empty `shouldBe` True
        (empty :: RBBag Int) ?<= add 1 empty `shouldBe` True

      it "non-empty > empty" $ do
        add 1 empty ?> empty `shouldBe` True
        add 1 empty ?>= empty `shouldBe` True

      it "reflexivity" $ do
        let bag = add 1 $ add 2 empty
        bag ?<= bag `shouldBe` True
        bag ?>= bag `shouldBe` True

    describe "Tree operations" $ do
      it "foldlTree test" $ do
        let bag = add 3 $ add 1 $ add 2 empty
        foldlTree (flip (:)) [] bag `shouldBe` [1, 2, 3]

      it "foldrTree test" $ do
        let bag = add 1 $ add 2 $ add 3 empty
        foldrTree (:) [] bag `shouldBe` [3, 2, 1]

      it "mapTree test" $ do
        let bag = add 1 $ add 2 empty
        let mapped = mapTree (+ 1) bag
        contains mapped 2 `shouldBe` True
        contains mapped 3 `shouldBe` True
        size mapped `shouldBe` 2

      it "filterTree tests" $ do
        let bag = add 1 $ add 2 $ add 3 $ add 4 empty
        let filtered = filterTree even bag
        contains filtered 2 `shouldBe` True
        contains filtered 4 `shouldBe` True
        contains filtered 1 `shouldBe` False
        contains filtered 3 `shouldBe` False
        size filtered `shouldBe` 2

    describe "Polymorphism" $ do
      it "different types tests" $ do
        let intBag = add (1 :: Int) empty
        let charBag = add 'a' empty
        let stringBag = add "hello" empty
        contains intBag 1 `shouldBe` True
        contains charBag 'a' `shouldBe` True
        contains stringBag "hello" `shouldBe` True

    describe "Property-based tests - Monoid properties" $ do
      it "left identity: empty <> x == x" $ do
        let bag = add 1 $ add 2 empty
        (empty <> bag) `shouldBe` bag

      it "right identity: x <> empty == x" $ do
        let bag = add 1 $ add 2 empty
        (bag <> empty) `shouldBe` bag

      it "associativity: (x <> y) <> z == x <> (y <> z)" $ do
        let x = add 1 $ add 2 empty
        let y = add 2 $ add 3 empty
        let z = add 3 $ add 4 empty
        ((x <> y) <> z) `shouldBe` (x <> (y <> z))

    describe "Additional simple test" $ do
      it "union with empty is identity" $ do
        let bag = add 1 $ add 2 $ add 3 empty
        union empty bag `shouldBe` bag
        union bag empty `shouldBe` bag
