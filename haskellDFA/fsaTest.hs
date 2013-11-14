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

-- equality operator for transitions
eq :: (Eq a, Eq b) => (a, b, c) -> (a, b, c) -> Bool
eq (a1, b1, _) (a2, b2, _) = if (a1 == a2) && (b1 == b2) then True else False

-- merge two transitions from and to the same states on differing inputs
merge :: Ord c => (a, b, Set c) -> (a, b, Set c) -> (a, b, Set c)
merge (a1, b1, c1) (a2, b2, c2) = (a1, b1, c1 `union` c2)

-- merge all the transitions from q_i to q_j
mergeTransSet :: Set (Int, Int, Set Char) -> Set (Int, Int, Set Char)
mergeTransSet a = 
    if size a == 1 then a
    else 
        Data.Set.fold merge q fst p `union` mergeTransSet (snd p)
        where 
            q = findMin a
            p = partition (eq q) a

-- create a list of final accepting states
getFinals :: Int -> Set Int
getFinals k = 
    singleton 0 `union` singleton k

main :: IO()
main =
    putStrLn $ showTreeWith False True $ (getDFAtrans 7 [0..6] [0..9]) `union` convertTransSet (getDropTrans 7)
