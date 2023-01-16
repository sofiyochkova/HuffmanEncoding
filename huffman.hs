
generateList :: String -> [Char] -> [(Char, Int)]
generateList "" _ = []
generateList (x:xs) res
    | x `notElem` res = (x, 1 + count x xs) : generateList xs (x:res)
    | otherwise = generateList xs res
  where count a str = length (filter (== a) str)


-- >>> generateList "abracadabra" []
-- [('a',5),('b',2),('r',2),('c',1),('d',1)]

data Tree a = Empty | Node {root :: a, leftTree :: Tree a, rightTree :: Tree a} | Leaf {c :: Char,  root :: a}
    deriving Read

--leaf :: a -> Char -> Tree a
--leaf a c = Leaf a c

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node a b c) = "(Tree " ++ show a ++ " (" ++ show b ++ ") " ++ "(" ++ show c ++ "))"
    show (Leaf c a) = "Leaf " ++ show a ++ " " ++ show c

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    Node a l1 r1 == Node b l2 r2 = a == b && l1 == l2 && r1 == r2
    Leaf s1 a == Leaf s2 b = a == b && s1 == s2
    _ == _ = False

instance Ord a => Ord (Tree a) where
    Empty `compare` Empty = EQ
    (Node r1 _ _) `compare` (Node r2 _ _) = r1 `compare` r2
    (Leaf _ a) `compare` (Leaf _ b) = a `compare` b
    _ `compare` _ = LT


createLeafList :: [(Char, Int)] -> [Tree Int]
createLeafList [] = []
createLeafList lst = map (uncurry Leaf) lst


removeMin :: Eq a => Tree a -> [Tree a] -> [Tree a]
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

createBinaryValues :: Tree Int -> String -> [(Char, String)]
createBinaryValues Empty _ = []
createBinaryValues (Node root l r) str = createBinaryValues l (str ++ "0") ++ createBinaryValues r (str ++ "1")
createBinaryValues (Leaf c root) str = [(c, str)]

encodeOriginalWord :: String -> [(Char, String)] -> String
encodeOriginalWord "" _ = ""
encodeOriginalWord word [] = word
encodeOriginalWord word (x:xs) = encodeOriginalWord (replaceLetter x word) xs 
    where replaceLetter :: (Char, String) -> String -> String
          replaceLetter _ "" = ""
          replaceLetter c (x:xs)
            | fst c == x = snd c ++ replaceLetter c xs
            | otherwise = x : replaceLetter c xs

huffmanEncode :: String -> (Tree Int, String)
huffmanEncode "" = (Empty, [])
huffmanEncode word = (huffTree, binaryString)
        where frequencyList = generateList word ""
              huffTree = huffmanTree $ createLeafList frequencyList
              binValues = createBinaryValues huffTree ""
              binaryString = encodeOriginalWord word binValues

-- >>> huffmanEncode "abracadabra"
-- ((Tree 11 ((Tree 6 ((Tree 4 ((Tree 2 (Leaf 1 'c') (Leaf 1 'd'))) (Leaf 2 'b'))) (Leaf 2 'r'))) (Leaf 5 'a')),"10010110000100011001011")


-- >>> huffmanTree $ createLeafList $ generateList "abracadabra" []
-- (Tree 11 ((Tree 6 ((Tree 4 ((Tree 2 (Leaf 1 'c') (Leaf 1 'd'))) (Leaf 2 'b'))) (Leaf 2 'r'))) (Leaf 5 'a'))

-- >>> createBinaryValues ( huffmanTree $ createLeafList $ generateList "abracadabra" []) ""
-- [('c',"0000"),('d',"0001"),('b',"001"),('r',"01"),('a',"1")]

-- >>> removeMin 1 $ createLeafList $ generateList "abracadabra" []
-- [(Tree 5 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 1 (Empty) (Empty))]

-- >>> createLeafList (generateList "abracadabra" [])
-- [(Tree 5 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 1 (Empty) (Empty)),(Tree 1 (Empty) (Empty))]

-- >>> map leaf [5,2,2,1,1]
-- [(Tree 5 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 2 (Empty) (Empty)),(Tree 1 (Empty) (Empty)),(Tree 1 (Empty) (Empty))]
