
generateList :: String -> [Char] -> [(Char, Int)]
generateList "" _ = []
generateList (x:xs) res
    | x `notElem` res = (x, 1 + count x xs) : generateList xs (x:res)
    | otherwise = generateList xs res
  where count a str = length (filter (== a) str)


-- >>> generateList "abracadabra" []
-- [('a',5),('b',2),('r',2),('c',1),('d',1)]

-- should put root
data Tree a = Empty | Node {root :: a, leftTree :: Tree a, rightTree :: Tree a}
    deriving Read

leaf :: a -> Tree a
leaf a = Node a Empty Empty

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node a b c) = "(Tree " ++ show a ++ " (" ++ show b ++ ") " ++ "(" ++ show c ++ "))"

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    Node a l1 r1 == Node b l2 r2 = a == b && l1 == l2 && r1 == r2
    _ == _ = False 

instance Ord a => Ord (Tree a) where
    Empty `compare` Empty = EQ
    (Node r1 _ _) `compare` (Node r2 _ _) = r1 `compare` r2
    _ `compare` _ = LT
    

createLeafList :: [(Char, Int)] -> [Tree Int]
createLeafList [] = []
createLeafList lst = map (leaf . snd) lst

removeMin :: Tree Int -> [Tree Int] -> [Tree Int]
removeMin _ [] = []
removeMin a (x:xs) = if a == x then xs 
    else x : removeMin a xs

--minIdk :: [Tree Int] -> Tree Int
--minIdk [] = Empty
--minIdk (x:xs) 
 
huffmanTree :: [Tree Int] -> Tree Int
huffmanTree [] = Empty
huffmanTree [a] = a
huffmanTree list = huffmanTree $ Node (root min1 + root min2) min1 min2 : removeMin min2 rest
    where min1 = minimum list
          min2 = minimum rest
          rest = removeMin min1 list

-- >>> huffmanTree 

-- >>> removeMin 1 $ createLeafList $ generateList "abracadabra" []
-- [(Tree 5 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 1 (Empty) (Empty))]

-- >>> createLeafList (generateList "abracadabra" [])
-- [(Tree 5 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 1 (Empty) (Empty)),(Tree 1 (Empty) (Empty))]

-- >>> map leaf [5,2,2,1,1]
-- [(Tree 5 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 1 (Empty) (Empty)),(Tree 1 (Empty) (Empty))]
