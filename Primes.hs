module Main (main) where

import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map

newtype Prime = Prime Integer deriving (Eq, Ord, Show)
newtype NonPrime = NonPrime Integer deriving (Eq, Ord, Show)

-- not actually the Sieve: really an inneficient trial division algorithm
-- non-infinite, expensive, bad
unfaithfulSieve :: [Integer] -> [Prime]
unfaithfulSieve [] = []
unfaithfulSieve (x : xs) =
    Prime x : unfaithfulSieve (filter (\y -> y `mod` x /= 0) xs)

slowPrimes :: [Prime]
slowPrimes = unfaithfulSieve [2 ..]

-- "Genuine Sieve of Eratosthenes"
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
-- TODO upgrade using a wheel, and priority queue rather than Map
sieve :: [Integer] -> Map NonPrime [Prime] -> [Prime]
sieve [] _ = []
sieve (x : xs) nextMultiples =
    case Map.lookup (NonPrime x) nextMultiples of
        Nothing ->
            let px = Prime x
             in px : sieve xs (Map.insertWith (++) (firstOddMultiple px) [px] nextMultiples)
        Just primesXIsAMultilpeOf ->
            let npx = NonPrime x
             in sieve xs $
                    foldl'
                        (\m p -> Map.insertWith (++) (nextOddMultipleAfter npx p) [p] m)
                        (Map.delete npx nextMultiples)
                        primesXIsAMultilpeOf
  where
    nextOddMultipleAfter (NonPrime after) (Prime p) =
        let a2p = after + 2 * p
            a3p = after + 3 * p
         in if odd a2p
                then NonPrime a2p
                else NonPrime a3p

    firstOddMultiple = nextOddMultipleAfter (NonPrime 0)

primes :: [Prime]
primes = Prime 2 : sieve [3, 5 ..] Map.empty

main :: IO ()
main = do
    -- print $ maximum $ take 30_000 slowPrimes -- ~16s
    print $ maximum $ take 1_000_000 primes -- ~14s (with -O2)
