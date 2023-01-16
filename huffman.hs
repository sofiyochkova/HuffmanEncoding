generateList :: String -> [Char] -> [(Char, Int)]
generateList "" _ = []
generateList (x:xs) res
    | x `notElem` res = (x, 1 + count x xs) : generateList xs (x:res)
    | otherwise = generateList xs res
  where count a str = length (filter (== a) str)

data Tree a = Empty | Node {root :: a, leftTree :: Tree a, rightTree :: Tree a} | Leaf {c :: Char,  root :: a}
    deriving Read

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

createHuffmanTreeFromList :: [Tree Int] -> Tree Int
createHuffmanTreeFromList [] = Empty
createHuffmanTreeFromList [a] = a
createHuffmanTreeFromList list = createHuffmanTreeFromList $ Node (root min1 + root min2) min1 min2 : removeMin min2 rest
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

-- трябва да взима и предикат за сравнение
huffmanEncode :: String -> (Tree Int, String)
huffmanEncode "" = (Empty, [])
huffmanEncode word = (huffmanTree, binaryString)
        where frequencyList = generateList word ""
              huffmanTree = createHuffmanTreeFromList $ createLeafList frequencyList
              binValues = createBinaryValues huffmanTree ""
              binaryString = encodeOriginalWord word binValues

huffmanDecode :: (Tree Int, String) -> String
huffmanDecode (originalTree, s) = helper (originalTree, s)
    where helper :: (Tree Int, String) -> String
          helper (Empty, _) = ""
          helper (Node root l r, "") = ""
          helper (Leaf c _, "") = [c]
          helper (Leaf c _, str) = c : helper (originalTree, str)
          helper (Node root l r, x:xs)
            | x == '0' = helper (l, xs)
            | x == '1' = helper (r, xs)
            | otherwise = "error"

-- >>> huffmanDecode (huffmanEncode "abracadabra") 
-- "abracadabra"

-- >>> huffmanEncode "abracadabra"
-- ((Tree 11 ((Tree 6 ((Tree 4 ((Tree 2 (Leaf 1 'c') (Leaf 1 'd'))) (Leaf 2 'b'))) (Leaf 2 'r'))) (Leaf 5 'a')),"10010110000100011001011")

-- >>> createHuffmanTreeFromList $ createLeafList $ generateList "abracadabra" []
-- (Tree 11 ((Tree 6 ((Tree 4 ((Tree 2 (Leaf 1 'c') (Leaf 1 'd'))) (Leaf 2 'b'))) (Leaf 2 'r'))) (Leaf 5 'a'))

-- >>> createBinaryValues ( createHuffmanTreeFromList $ createLeafList $ generateList "abracadabra" []) ""
-- [('c',"0000"),('d',"0001"),('b',"001"),('r',"01"),('a',"1")]
