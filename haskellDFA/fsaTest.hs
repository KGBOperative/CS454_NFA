import NFAtoDFA

-- create a transition table based on the value of the divisor
getDFAtrans :: (Integer k a, Int c) => k -> [a] -> [c] -> [(a, b, d)]
getDFAtrans _ _ [] = []
getDFAtrans _ [] _ = []
getDFAtrans k [a] [c] = (a, (a * 10 + c) `mod` k, intToChar c) : (a + k, ((a * 10 + c) `mod` k) + k, intToChar c)
getDFAtrans k [a] (c:cs) = (getDFAtrans k a c) : (getDFAtrans k a cs)
getDFAtrans k (a:as) (c:cs) = (getDFAtrans k a (c:cs)) : (getDFAtrans k as (c:cs))

-- create a list of tuples for the dropped digit transitions
getDropTrans :: (Integer a) => a -> [(a, a, a)]
getDropTrans a = zip3 [0..(a-1)] [a..(2*a-1)] (rep [0..9] a)
    where rep a 0 = []
          rep a b = a : rep a (b-1)

-- convert the transition table from the dropped-digit tuples
convertDropTrans :: (Int c) => [(a, b, c)] -> [(a, b, d)]
convertDropTrans [(a, b, c)] = [(a, b, intToDigit c)]
convertDropTrans [a:as] = convertDropTrans a : convertDropTrans as

-- create a list of final accepting states
getFinals :: (Integer k) => k -> [k]
getFinals = [0, k]

-- convert a list of Int to Char
convertInts :: (Int a) => [a] -> [b]
convertInts [] = []
convertInts [a] = intToDigit a
convertInts (a:as) = convertInts a : convertInts as

nearlyDivSeven = FiniteMachine transTable alpha start finals
    where transTable = getDFAtrans 7 [0..6] [0..9]
          alpha = convertInts [0..9]
          start = 0 
          finals = getFinals 7

main :: IO ()
main args = do {
     
}
