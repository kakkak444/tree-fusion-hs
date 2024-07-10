{-# LANGUAGE RankNTypes #-}

data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    {-# INLINE fmap #-}
    fmap = mapTree

buildTree :: (forall b. (a -> b -> b -> b) -> b -> b) -> Tree a
{-# INLINE [1] buildTree #-}
buildTree g = g Node Leaf

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
{-# INLINE [0] foldTree #-}
foldTree f n Leaf = n
foldTree f n (Node a left right) = f a (foldTree f n left) (foldTree f n right)

{-# RULES
"fold/build" forall k z (g :: forall b. (a -> b -> b -> b) -> b -> b).
             foldTree k z (buildTree g) = g k z
    #-}

mapTree :: (a -> b) -> Tree a -> Tree b
{-# NOINLINE [0] mapTree #-}
mapTree f Leaf = Leaf
mapTree f (Node a left right) = Node (f a) (mapTree f left) (mapTree f right)

mapFB :: (c -> b -> b -> b) -> (a -> c) -> a -> b -> b -> b
{-# INLINE [0] mapFB #-}
mapFB c f = \x left right-> c (f x) left right

{-# RULES
"map"     [~1] forall f tree. mapTree f tree = buildTree (\c n -> foldTree (mapFB c f) n tree)
"mapTree" [1]  forall f. foldTree (mapFB Node f) Leaf = mapTree f
"mapFB"        forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id"     forall c. mapFB c (\x -> x) = c
    #-}


sumTree :: Int
{-# NOINLINE sumTree #-}
sumTree = foldTree (\a b c -> a + b + c) 0 (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)))

sumTree' :: Int
{-# NOINLINE sumTree' #-}
sumTree' = foldTree (\a b c -> a + b + c) 0 tree
  where
    tree :: Tree Int
    tree = buildTree (\c n -> c 1 (c 2 n n) (c 3 n (c 4 n n)))

sumList :: Int
{-# NOINLINE sumList #-}
sumList = foldr (+) 0 [1, 2, 3, 4]

sumTree'' :: Int
{-# NOINLINE sumTree'' #-}
sumTree''= foldTree (\a b c -> a + b + c) 0 (mapTree (*2) tree)
  where
    tree :: Tree Int
    tree = buildTree (\c n -> c 1 (c 2 n n) (c 3 n (c 4 n n)))

sumList' :: Int
sumList' = foldr (+) 0 (take 4 (map (*2) [1..10]))

main :: IO ()
main = do
    print sumTree
    print sumTree'
    print sumList
    print sumTree''
    print sumList'

