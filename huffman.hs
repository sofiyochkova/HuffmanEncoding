-- създаваме списък на броя на срещанията на всеки символ
generateFrequencyList :: String -> [Char] -> [(Char, Int)]
generateFrequencyList "" _ = []
generateFrequencyList (x:xs) res
    | x `notElem` res = (x, 1 + count x xs) : generateFrequencyList xs (x:res)
    | otherwise = generateFrequencyList xs res
  where count a str = length (filter (== a) str)

data Tree a = Empty | Node {root :: a, leftTree :: Tree a, rightTree :: Tree a} | Leaf {c :: Char,  root :: a}
   deriving (Show, Read) 

-- алтернативно извеждане на дървото
parseTree :: (Show a) => Tree a -> String
parseTree Empty = "Empty"
parseTree (Node a b c) = "(Tree " ++ show a ++ " (" ++ parseTree b ++ ") " ++ "(" ++ parseTree c ++ "))"
parseTree (Leaf c a) = "Leaf " ++ show a ++ " " ++ show c

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

-- от създадения честотен списък правим списък от отделните листа на дървото
createLeafList :: [(Char, a)] -> [Tree a]
createLeafList [] = []
createLeafList lst = map (uncurry Leaf) lst

-- от списъка с листа създаваме Хъфманово дърво
createHuffmanTreeFromList :: (Num a, Ord a) => [Tree a] -> Tree a
createHuffmanTreeFromList [] = Empty
createHuffmanTreeFromList [a] = a
createHuffmanTreeFromList list = createHuffmanTreeFromList $ Node (root min1 + root min2) min1 min2 : removeMin min2 rest
    where min1 = minimum list
          min2 = minimum rest
          rest = removeMin min1 list

          removeMin :: Eq a => Tree a -> [Tree a] -> [Tree a]
          removeMin _ [] = []
          removeMin a (x:xs) = if a == x then xs
            else x : removeMin a xs

-- обхождаме Хъфмановото дърво и от него създаваме списък от символ и съответния му булев низ
generateBinaryValueList :: Tree a -> String -> [(Char, String)]
generateBinaryValueList Empty _ = []
generateBinaryValueList (Node root l r) str = generateBinaryValueList l (str ++ "0") ++ generateBinaryValueList r (str ++ "1")
generateBinaryValueList (Leaf c root) str = [(c, str)]

-- с помощта на списъка кодираме думата, получаваме булев низ
encodeOriginalWord :: String -> [(Char, String)] -> String
encodeOriginalWord "" _ = ""
encodeOriginalWord word [] = word
encodeOriginalWord word (x:xs) = encodeOriginalWord (replaceLetter x word) xs 
    where replaceLetter :: (Char, String) -> String -> String
          replaceLetter _ "" = ""
          replaceLetter c (x:xs)
            | fst c == x = snd c ++ replaceLetter c xs
            | otherwise = x : replaceLetter c xs

-- кодиране на дума
huffmanEncode :: String -> (Tree Int, String)
huffmanEncode "" = (Empty, [])
huffmanEncode word = (huffmanTree, binaryString)
        where frequencyList = generateFrequencyList word ""
              huffmanTree = createHuffmanTreeFromList $ createLeafList frequencyList
              binValues = generateBinaryValueList huffmanTree ""
              binaryString = encodeOriginalWord word binValues

-- декодиране по подадено Хъфманово дърво и бинарен низ
huffmanDecode :: (Tree a, String) -> String
huffmanDecode (originalTree, s) = helper (originalTree, s)
    where helper :: (Tree a, String) -> String
          helper (Empty, _) = ""
          helper (Node root l r, "") = ""
          helper (Leaf c _, "") = [c]
          helper (Leaf c _, str) = c : helper (originalTree, str)
          helper (Node root l r, x:xs)
            | x == '0' = helper (l, xs)
            | x == '1' = helper (r, xs)
            | otherwise = error "Invalid string!"
