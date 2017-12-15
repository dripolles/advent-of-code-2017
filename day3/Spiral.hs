import Prelude hiding (Left, Right)
import qualified Data.Map.Strict as Map

data Direction = Up | Down | Left | Right deriving (Eq, Show)

directionCounts :: [Int]
directionCounts = foldr (\x acc -> (x:x:acc)) [] [1..]

directions :: [Direction]
directions = directions' Right directionCounts

directions' :: Direction -> [Int] -> [Direction]
directions' d (x:xs) = replicate x d ++ directions' (turnFrom d) xs
directions' _ [] = []

move :: Direction -> (Int, Int) -> (Int, Int)
move d (x,y) = case d of
    Up -> (x,y+1)
    Down -> (x,y-1)
    Left -> (x-1, y)
    Right -> (x+1, y)

turnFrom :: Direction -> Direction
turnFrom d = case d of
    Up -> Left
    Down -> Right
    Left -> Down
    Right -> Up

spiral :: [(Int, Int)]
spiral = start : runDirections start directions where start = (0,0)

runDirections :: (Int, Int) -> [Direction] -> [(Int, Int)]
runDirections s (d:ds) =  s' : runDirections s' ds where s' = move d s
runDirections _ [] = []

findStep :: Int -> (Int, Int)
findStep n = spiral !! (n-1)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (c,d) = abs(a-c) + abs(b-d)

stressValues :: [Int]
stressValues = fillValues (Map.singleton (0,0) 1) (tail spiral)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x-1, y+1), (x, y+1), (x+1, y+1), (x-1, y), (x+1, y), (x-1, y-1), (x, y-1), (x+1, y-1)]

fillValues ::  (Map.Map (Int, Int) Int) -> [(Int, Int)] -> [Int]
fillValues m (c:cs) = s : fillValues m' cs where
    s  = sum $ map getValue (neighbours c)
    getValue x = Map.findWithDefault 0 x m
    m' = Map.insert c s m
fillValues _ [] = []

main = do
    let step = findStep 312051
    print step
    let dist = manhattan step (0,0)
    print dist
    print $ head (dropWhile (\x -> x < 312051) stressValues)
