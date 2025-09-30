# lab2


## Вариант 
**rb-bag**


## Требования:

1. Функции:

    * добавление и удаление элементов;
    ```haskell
    add :: Ord a => a -> RBBag a -> RBBag a
    add x t = case ins x t of
    Node _ val left right -> Node Black val left right
    Empty -> Empty

    delete :: (Ord a) => a -> RBBag a -> RBBag a
    delete x t = makeBlack $ del x t

    delete' :: (Ord a) => a -> RBBag a -> RBBag a
    delete' x t = makeBlack $ annihilate x t
    ```
    * фильтрация;
    ```haskell
    filterTree :: (Ord a) => (a -> Bool) -> RBBag a -> RBBag a
    filterTree pred = foldrTree (\x acc -> if pred x then add x acc else acc) empty
    ```
    * отображение (map);
    ```haskell
    mapTree :: (Ord b) => (a -> b) -> RBBag a -> RBBag b
    mapTree f = foldrTree (\x acc -> add (f x) acc) empty
    ```
    * свертки (левая и правая);
    ```haskell
    foldlTree :: (b -> a -> b) -> b -> RBBag a -> b
    foldlTree _ acc Empty = acc
    foldlTree f acc (Node _ val left right) = foldlTree f (f (foldlTree f acc right) val) left

    foldrTree :: (a -> b -> b) -> b -> RBBag a -> b
    foldrTree _ acc Empty = acc
    foldrTree f acc (Node _ val left right) = foldrTree f (f val (foldrTree f acc left)) right
    ```
    * структура должна быть моноидом.
    ```haskell
    instance Ord a => Semigroup (RBBag a) where
        (<>) = union

    instance Ord a => Monoid (RBBag a) where
        mempty = empty
        mappend = (<>)
    ```

2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
7. Обратите внимание:

    * API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
    * Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.

    ```haskell
    (?=) :: (Ord a) => RBBag a -> RBBag a -> Bool
    tree1 ?= tree2 = comp tree1 tree2 == EQ

    (?/=) :: (Ord a) => RBBag a -> RBBag a -> Bool
    tree1 ?/= tree2 = comp tree1 tree2 /= EQ

    (?<) :: (Ord a) => RBBag a -> RBBag a -> Bool
    tree1 ?< tree2 = comp tree1 tree2 == LT

    (?>) :: (Ord a) => RBBag a -> RBBag a -> Bool
    tree1 ?> tree2 = comp tree1 tree2 == GT

    (?<=) :: (Ord a) => RBBag a -> RBBag a -> Bool
    tree1 ?<= tree2 = case comp tree1 tree2 of
        LT -> True
        EQ -> True
        GT -> False

    (?>=) :: (Ord a) => RBBag a -> RBBag a -> Bool
    tree1 ?>= tree2 = case comp tree1 tree2 of
        GT -> True
        EQ -> True
        LT -> False
    ```

