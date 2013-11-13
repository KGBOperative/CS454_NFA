import Data.Char
import Data.Set 
import System.IO
import Data.Tree

-- create a transition table based on the value of the divisor
getDFAtrans :: Int -> [Int] -> [Int] -> Set (Int, Int, Set Char)
getDFAtrans k [a] [c] = 
    singleton ((a, (a * 10 + c) `mod` k, singleton (intToDigit c))) `union`
    singleton ((a + k, ((a * 10 + c) `mod` k) + k, singleton (intToDigit c)))
getDFAtrans k [a] (c:cs) = 
    singleton ((a, (a * 10 + c) `mod` k, singleton (intToDigit c))) `union`
    singleton ((a + k, ((a * 10 + c) `mod` k) + k, singleton (intToDigit c))) `union`
    (getDFAtrans k [a] cs) 
getDFAtrans k (a:as) (c:cs) = 
    (getDFAtrans k [a] (c:cs)) `union` (getDFAtrans k as (c:cs))

{-- old, list verion
getDFAtrans :: Int -> [Int] -> [Int] -> [(Int, Int, [Char])]
getDFAtrans k [a] [c] = [(a, (a * 10 + c) `mod` k, [intToDigit c])
                       , (a + k, ((a * 10 + c) `mod` k) + k, [intToDigit c])]
getDFAtrans k [a] (c:cs) = (a, (a * 10 + c) `mod` k, [intToDigit c])
                       : (a + k, ((a * 10 + c) `mod` k) + k, [intToDigit c]) 
                       : (getDFAtrans k [a] cs)
getDFAtrans k (a:as) (c:cs) = (getDFAtrans k [a] (c:cs)) ++ (getDFAtrans k as (c:cs))
--}

-- create a list of tuples for the dropped digit transitions
getDropTrans :: Int -> Set ((Int, Int, [Int]))
getDropTrans a = 
    fromList $ zip3 [0..(a-1)] [a..(2*a-1)] (rep [0..9] a)

-- replicate list a n number of times
rep :: [a] -> Int -> [[a]]
rep a 0 = []
rep a n = 
    a : rep a (n-1)

-- convert a list of Ints to a list of Chars
intsToDigits :: [Int] -> Set Char
intsToDigits [a] = singleton (intToDigit a)
intsToDigits (a:as) = 
    singleton (intToDigit a) `union` (intsToDigits as)

-- convert the transition table from the dropped-digit tuples
convertTransSet :: Set (Int, Int, [Int]) -> Set (Int, Int, Set Char)
convertTransSet a = 
    if Data.Set.null a then empty
    else singleton (convertTrans (findMin a)) `union` convertTransSet (deleteMin a)
        where convertTrans (a, b, c) = (a, b, intsToDigits c)

-- create a list of final accepting states
getFinals :: Int -> Set Int
getFinals k = 
    singleton 0 `union` singleton k

{-- combineDelta :: (a, b, [c]) -> (a, b, [c]) -> (a, b, [c])
-- combineDelta (a1, b1, (
-- reduce the number of transitions
reduceDelta :: [(Int, Int, [Char])] -> [(Int, Int, [Char])]
reduceDelta [(a, b, (c:cs))] = 

-- define NFA as a data type
data NFA = NFA { states :: [Int]
               , sigma :: [Char]
               , delta :: [(Int, Int, [Char])] 
               , initState :: Int 
               , finalState :: [Int] 
               }
--}
main :: IO()
main =
    putStrLn $ showTreeWith False True $ (getDFAtrans 3 [0..2] [0..9]) `union` convertTransSet (getDropTrans 3)
