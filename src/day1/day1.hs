{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ( arr, (>>>) )
import Data.List ( sort, transpose )
import Data.Text ( pack, split, unpack, Text )
import Text.Read ( readMaybe )

ingestLists :: Text -> Maybe [[Int]]
ingestLists = arr $ split (== '\n')
    >>> map (words . unpack)
    >>> mapM (mapM readMaybe)

similarity :: [Int] -> Int -> Int
similarity haystack needle =
    foldl (+?) 0 haystack
    where
        x +? y = if y == needle
            then x + y
            else x

commonCode :: String -> Maybe [[Int]]
commonCode x = do
    dat <- (ingestLists . pack) x
    return $ map sort (transpose dat)

part1 :: String -> Maybe Int
part1 x = do
    dat <- commonCode x
    let diffs = zipWith (\x y -> abs $ x - y) (head dat) (last dat)
    return $ sum diffs

part2 :: String -> Maybe Int
part2 x = do
    dat <- commonCode x
    let scores = map (similarity $ last dat) (head dat)
    return $ sum scores

main :: IO ()
main = do
    rawText <- readFile "input.txt"
    print $ "The solution to part 1 is: " ++ show (part1 rawText)
    print $ "The solution to part 2 is: " ++ show (part2 rawText)

