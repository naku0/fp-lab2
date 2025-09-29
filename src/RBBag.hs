module RBBag (
    RBBag, 
    empty, 
    add, 
    delete, 
    union, 
    size, 
    contains, 
    count, 
    logTree, 
    delete',
    isEmpty, 
    balance, 
    fuse, 
    extractMin,
    mapTree,
    filterTree,
    foldlTree,
    foldrTree,
) where

import Data.Monoid
import Data.Semigroup

data Color = Red | Black deriving (Show, Eq)
data RBBag a = Empty | Node Color a (RBBag a) (RBBag a) deriving (Show, Eq)

-- api frontend

empty :: RBBag a
empty = Empty

add :: Ord a => a -> RBBag a -> RBBag a
add x t = case ins x t of
    Node _ val left right -> Node Black val left right
    Empty -> Empty

delete :: (Ord a) => a -> RBBag a -> RBBag a
delete x t = makeBlack $ del x t

delete' :: (Ord a) => a -> RBBag a -> RBBag a
delete' x t = makeBlack $ annihilate x t

union :: (Ord a) => RBBag a -> RBBag a -> RBBag a
union Empty bag = bag
union bag Empty = bag
union (Node _ val left right) bag2 = add val (union left (union right bag2))

size :: RBBag a -> Int
size Empty = 0
size (Node _ _ left right) = 1 + size left + size right

contains :: (Ord a) => RBBag a -> a -> Bool
contains Empty _ = False
contains (Node _ val left right) x
    | x < val = contains left x
    | x > val = contains right x
    | otherwise = True

count :: (Ord a) => a -> RBBag a -> Int
count _ Empty = 0
count x (Node _ val left right)
    | x < val   = count x left
    | x > val   = count x right
    | otherwise = 1 + count x left + count x right

logTree :: Show a => RBBag a -> IO ()
logTree = putStrLn . drawTree

instance Ord a => Semigroup (RBBag a) where
    (<>) = union

instance Ord a => Monoid (RBBag a) where
    mempty = empty
    mappend = (<>)

foldlTree :: (b -> a -> b) -> b -> RBBag a -> b
foldlTree _ acc Empty = acc
foldlTree f acc (Node _ val left right) = foldlTree f (f (foldlTree f acc right) val) left

foldrTree :: (a -> b -> b) -> b -> RBBag a -> b
foldrTree _ acc Empty = acc
foldrTree f acc (Node _ val left right) = foldrTree f (f val (foldrTree f acc left)) right

mapTree :: (Ord b) => (a -> b) -> RBBag a -> RBBag b
mapTree f = foldrTree (\x acc -> add (f x) acc) empty

filterTree :: (Ord a) => (a -> Bool) -> RBBag a -> RBBag a
filterTree pred = foldrTree (\x acc -> if pred x then add x acc else acc) empty

isEmpty :: RBBag a -> Bool
isEmpty Empty = True
isEmpty _     = False

makeBlack :: RBBag a -> RBBag a
makeBlack Empty = Empty
makeBlack (Node _ val left right) = Node Black val left right

balance :: Color -> a -> RBBag a -> RBBag a -> RBBag a
balance Black x (Node Red y (Node Red z a b) c) d = Node Red y (Node Black z a b) (Node Black x c d)
balance Black x (Node Red y a (Node Red z b c)) d = Node Red z (Node Black y a b) (Node Black x c d)
balance Black x a (Node Red y (Node Red z b c) d) = Node Red z (Node Black x a b) (Node Black y c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance color val left right = Node color val left right

fuse :: RBBag a -> RBBag a -> RBBag a
fuse Empty t2 = t2
fuse t1 Empty = t1
fuse t1 t2 = case extractMin t2 of
    Just (minVal, t2') -> balance Red minVal t1 t2'
    Nothing -> t1

extractMin :: RBBag a -> Maybe (a, RBBag a)
extractMin Empty = Nothing
extractMin (Node _ val Empty right) = Just (val, right)
extractMin (Node color val left right) = 
    case extractMin left of
        Just (minVal, left') -> Just (minVal, balance color val left' right)
        Nothing -> Nothing

-- api backend

ins :: Ord a => a -> RBBag a -> RBBag a  
ins x Empty = Node Red x Empty Empty
ins x (Node color y left right)
    | x < y     = balance color y (ins x left) right
    | x > y     = balance color y left (ins x right)
    | otherwise = balance color y left (ins x right)

del :: (Ord a) => a -> RBBag a -> RBBag a
del _ Empty = Empty
del x (Node color val left right)
    | x < val  = balance color val (del x left) right
    | x > val  = balance color val left (del x right)
    | otherwise = fuse left right

annihilate :: (Ord a) => a -> RBBag a -> RBBag a
annihilate _ Empty = Empty
annihilate x (Node color val left right)
    | x < val  = balance color val (annihilate x left) right
    | x > val  = balance color val left (annihilate x right)
    | otherwise = fuse (annihilate x left) (annihilate x right)

delL :: (Ord a) => a -> RBBag a -> RBBag a
delL _ Empty = Empty
delL x (Node color val left right) = balance color val (del x left) right

delR :: (Ord a) => a -> RBBag a -> RBBag a
delR _ Empty = Empty
delR x (Node color val left right) = balance color val left (del x right)

balanceMerge :: RBBag a -> RBBag a
balanceMerge Empty = Empty
balanceMerge (Node color val left right) = balance color val left right

drawTree :: Show a => RBBag a -> String
drawTree Empty = "Empty"
drawTree tree = unlines (draw tree True "")
  where
    draw Empty _ _ = []
    draw (Node color val left right) isRight indent =
        let (symbol, nextIndent) = if isRight then ("┌── ", indent ++ "│   ") else ("└── ", indent ++ "    ")
            colorStr = case color of 
                Red -> "🔴 "
                Black -> "⚫ "
            nodeLine = indent ++ symbol ++ colorStr ++ show val
            leftLines = draw left False nextIndent
            rightLines = draw right True nextIndent
        in rightLines ++ [nodeLine] ++ leftLines