module Main (main) where

import Unsafe.Coerce (unsafeCoerce)

-- Y combinator
-- recursive definition (defeats the point)
yRec :: (a -> a) -> a
yRec f = f (yRec f)

-- Y combinator
-- non-recursive definition (unsafeCoerce needed as type is not representable in Haskell)
y :: (a -> a) -> a
y f = f ((\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x)))

fib :: (Num a, Eq a) => a -> a
fib n =
    let fib' _ 0 = 0
        fib' _ 1 = 1
        fib' f n = f (n - 1) + f (n - 2) -- non-recursive
     in y fib' n -- create recursion using the Y combinator

countToN :: (Num a, Eq a) => a -> [a]
countToN n =
    -- bake n into a (non-recursive) closure
    let countToN' f [] = f [0]
        countToN' f (x : xs) = if x == n then x : xs else f $ x + 1 : x : xs
     in -- use the Y combinator to create recursion using the closure
        reverse $ y countToN' []

unfaithfulSieve :: Integral a => [a] -> [a]
unfaithfulSieve xs =
    let unfaithfulSieve' _ [] = []
        unfaithfulSieve' f (x : xs) =
            x : f (filter (\y -> y `mod` x /= 0) xs)
     in y unfaithfulSieve' xs

main :: IO ()
main = do
    print $ fib 7
    print $ countToN 25
    print $ unfaithfulSieve [2 .. 100]
