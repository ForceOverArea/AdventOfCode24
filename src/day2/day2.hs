{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding ( words )
import Control.Arrow ( arr, (>>>) )
import Data.Text ( pack, split, unpack, words, Text )
import Text.Read ( readMaybe )
import Control.Monad ( foldM )
import Data.Bits ( Bits(xor) )
import Data.Either ( isRight )

reports :: String -> Either String [[Int]]
reports = arr $ split (== '\n') . pack
    >>> mapM (mapM (readMaybe . unpack) . words)
    >>> \case
            Just x -> Right x
            Nothing -> Left "Nothing"

safeAscend :: (Ord a, Num a, Show a) => a -> a -> Either String a
safeAscend x y = if x < y && (y - x <= 3)
    then Right y
    else Left $ show x
        ++ " is NOT less than "
        ++ show y
        ++ " by at most 3"

safeDescend :: (Ord a, Num a, Show a) => a -> a -> Either String a
safeDescend x y = if x > y && (x - y <= 3)
    then Right y
    else Left $ show x
        ++ " is NOT greater than "
        ++ show y
        ++ " by at most 3"

(?+) :: Num a => a -> Bool -> a
x ?+ True = x + 1
x ?+ False = x

isSafe :: [Int] -> Bool
isSafe [] = False
isSafe report = checkWith safeAscend || checkWith safeDescend
    where
        checkWith x = isRight (foldM x (head report) (tail report))

isSafeDampened :: [Int] -> Bool
isSafeDampened [] = False
isSafeDampened report = isSafe report || tryDampen report 
    where
        tryDampen :: [Int] -> Bool
        tryDampen r =
            let dampened = map (removeNth r) [0..length r]
            in 0 < foldl (?+) 0 (map isSafe dampened)

        removeNth :: [a] -> Int -> [a]
        removeNth [] _  = []
        removeNth xs n = take (n-1) xs ++ drop n xs

part1 :: Num b => String -> Either String b
part1 x = do
    reps <- reports x
    let safeOrUnsafe = map isSafe reps
    return $ foldl (?+) 0 safeOrUnsafe

part2 :: Num b => String -> Either String b
part2 x = do
    reps <- reports x
    let safeOrUnsafe = map isSafeDampened reps
    return $ foldl (?+) 0 safeOrUnsafe

main :: IO ()
main = do
    rawText <- readFile "input.txt"
    print $ part1 rawText
    print $ part2 rawText