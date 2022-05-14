module Main where

import Data.Char
import System.Environment
import Functions


main :: IO ()
main =
  do
    args <- getArgs
    
    case args of
      [] ->
        fail "Insufficient arguments: no arguments provided"
      [x] ->
        fail "Insufficient arguments: one argument provided"
      ("Read":filename:[]) ->
        do
          networkString <- readFile filename
          let networkList = convertToList networkString
          let writeString = buildString networkList
          writeFile "network.txt" (writeString)
      ("Run":filename:sequenceStr:[]) ->
        do
          networkString <- readFile filename
          let networkList = convertToList networkString
          let sequence = read sequenceStr :: [Int]
          let sortedSequence = applyComparator networkList sequence
          putStrLn (show sortedSequence)
      ("Parallel":filename:[]) ->
        do
          networkString <- readFile filename
          let networkList = convertToList networkString
          let parallelList = parallelize networkList
          let parallelString = buildParallelString parallelList
          writeFile "parallel.txt" (parallelString)
      ("Sorting":filename:[]) ->
        do
          networkString <- readFile filename
          let networkList = convertToList networkString
          let zeroOneList = create01 (numWires networkList 0)
          putStrLn (show(isSortingNetwork networkList zeroOneList))
      ("Create":number:[]) ->
        do
          let n = read number :: Int
          let networkList = createNetwork n
          let networkString = buildParallelString networkList
          writeFile "parallel.txt" (networkString)
      (x:xs) ->
        do
          putStrLn "Insufficient arguments: command not recognized."