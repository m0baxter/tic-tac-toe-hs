
module Board where


data Marker = X | O | Blank
               deriving Eq

instance Show Marker where
   show X = "X"
   show O = "O"
   show Blank = " "

newtype Board = Board [Marker]
                 deriving Eq

instance Show Board where
   show (Board b) = "   |   |   \n" ++
                    " " ++ show (b !! 0) ++ " | " ++ show (b !! 1) ++ " | " ++ show (b !! 2) ++ " \n" ++
                    "___|___|___\n" ++
                    "   |   |   \n" ++
                    " " ++ show (b !! 3) ++ " | " ++ show (b !! 4) ++ " | " ++ show (b !! 5) ++ " \n" ++
                    "___|___|___\n" ++
                    "   |   |   \n" ++
                    " " ++ show (b !! 6) ++ " | " ++ show (b !! 7) ++ " | " ++ show (b !! 8) ++ " \n" ++
                    "   |   |   "


newBoard :: Board
newBoard = Board $ take 9 $ repeat Blank


mkMove :: Board -> Marker -> Int -> Maybe Board
mkMove (Board bs) m n | bs !! n == Blank = Just ( Board $ replace bs m n )
                      | otherwise = Nothing


gameWon :: Board -> Bool
gameWon (Board b) = or triples
   where triples = [ threeInARow (b !! 0) (b !! 1) (b !! 2),
                     threeInARow (b !! 3) (b !! 4) (b !! 5),
                     threeInARow (b !! 6) (b !! 7) (b !! 8),
                     threeInARow (b !! 0) (b !! 3) (b !! 6),
                     threeInARow (b !! 1) (b !! 4) (b !! 7),
                     threeInARow (b !! 2) (b !! 5) (b !! 8),
                     threeInARow (b !! 0) (b !! 4) (b !! 8),
                     threeInARow (b !! 2) (b !! 4) (b !! 6) ]


threeInARow :: Marker -> Marker -> Marker -> Bool
threeInARow m1 m2 m3 | anyBlank m1 m2 m3 = False
                     | otherwise         = m1 == m2 && m2 == m3 && m3 == m1


anyBlank :: Marker -> Marker -> Marker -> Bool
anyBlank m1 m2 m3 = any (==Blank) [m1,m2,m3]


replace :: [a] -> a -> Int -> [a]
replace ls a n | n < length ls && n >= 0 = take (n) ls ++ [a] ++ drop (n+1) ls
               | otherwise = error "Index to replace out of bounds."

