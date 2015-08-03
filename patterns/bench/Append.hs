module Main where
import Mutants

main :: IO ()
main = ex1 12 3000

-- Example 1
ex1 k n = run2 k (map ($ n) [assoc, single, nil, appends, revdist, drops, takes]) (++)

assoc, single, nil, revdist, appends, drops, takes :: Int -> ([Bool] -> [Bool] -> [Bool]) -> Bool

assoc k (+)    = runTests k $ \(x,y,z) -> x + (y + z) == (x + y) + z

single k (+)   = runTests k $ \(x,xs) -> [x] + xs == x:xs

nil k (+)      = runTests k $ \xs -> [] + xs == xs

revdist k (+)  = runTests k $ \(xs,ys) -> reverse ys + reverse xs == reverse (xs + ys)

appends k (+)  = runTests k $ \(xs,ys) -> xs + ys == xs ++ ys

drops k (+)    = runTests k $ \(xs,ys) -> drop (length xs) (xs + ys) == ys

takes k (+)    = runTests k $ \(xs,ys) -> take (length xs) (xs + ys) == xs
