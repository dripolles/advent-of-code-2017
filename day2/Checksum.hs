checksum :: String -> Int
checksum = sum . map lineChecksum . lines

lineChecksum :: String -> Int
lineChecksum s = abs $ x - y where
    (x,y) = minMax $ map read $ words s

minMax :: [Int] -> (Int, Int)
minMax (x:xs) = foldl updateMinMax (x,x) xs where
    updateMinMax (x,y) x' = (min x x', max y x')

main = do
    contents <- readFile "checksum.txt"
    print $ checksum contents
