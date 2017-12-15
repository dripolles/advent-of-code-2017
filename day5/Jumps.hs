import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

type UpdateJumpValue = Int -> Int

evalJumpsM :: PrimMonad m
    => M.MVector (PrimState m) Int
    -> UpdateJumpValue
    -> Int -- position
    -> m Int -- steps taken

evalJumpsM mv f p = do
    let size = M.length mv
        jump mv p count
            | p < 0 || p >= size = return count
            | otherwise = do
                diff <- M.unsafeRead mv p
                let p' = p + diff
                M.unsafeWrite mv p (f diff)
                jump mv p' (count+1)

    jump mv 0 0

evalProblem :: (PrimMonad m) => V.Vector Int -> UpdateJumpValue -> m Int
evalProblem v f = do
    numbers <- V.thaw v
    evalJumpsM numbers f 0

main = do
    contents <- readFile "jumps.txt"
    let numberList = V.fromList $ map read (lines contents) :: V.Vector Int
    problem1 <- evalProblem numberList (+1)
    print problem1
    problem2 <- evalProblem numberList (\x -> if x >= 3 then (x-1) else (x+1))
    print problem2
