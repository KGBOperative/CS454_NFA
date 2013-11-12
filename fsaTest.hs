import Data.Char
import Data.Set 
import Data.Tree
import System.IO

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
getNFATrans :: Int -> Set ((Int, Int, [Int]))
getNFATrans a = 
    fromList $ zip3 [0..(a-1)] [a..(2*a-1)] (rep [0..9] a)

-- replicate list a n number of times
rep :: [a] -> Int -> [[a]]
rep a 0 = []
rep a n = 
    a : rep a (n-1)

-- convert a list of Ints to a list of Chars
intsToDigits :: [Int] -> Set Char
intsToDigits [] = empty
intsToDigits (a:as) = 
    if Data.Set.null x then singleton (intToDigit a)
    else singleton (intToDigit a) `union` x
        where x = intsToDigits as

-- convert the transition table from the dropped-digit tuples
convertTransSet :: Set (Int, Int, [Int]) -> Set (Int, Int, Set Char)
convertTransSet a = 
    if Data.Set.size a == 1 then singleton (convertTrans (findMin a))
    else singleton (convertTrans (findMin a)) `union` convertTransSet (deleteMin a)
        where convertTrans (a, b, c) = (a, b, intsToDigits c)

-- merge transitions that have the same initial and final state
mergeDelta :: Ord a => (a, a, Set b) -> (a, a, Set b) -> Set (a, a, Set b)
mergeDelta (a1, b1, c1) (a2, b2, c2) =
    if a1 == a2 && b1 == b2 then singleton (a1, b1, (c1 `union` c2))
    else singleton (a1, b1, c1) `union` singleton (a2, b2, c2)

-- create a list of final accepting states
getFinals :: Int -> Set Int
getFinals k = 
    singleton 0 `union` singleton k

main =
    putStrLn $ showTreeWith False True $ (getDFAtrans 2 [0,1] [0..9]) `union` convertTransSet (getNFATrans 2)
