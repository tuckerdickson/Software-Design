module Functions where

import System.Environment

{-------------------------------------------------------------}
{-------------------------- Part II --------------------------}
{-------------------------------------------------------------}


{---------------------------------------------------------------
-  description: this function uses the built-in read function to
-               convert a string to a list of (Int,Int) pairs.
-  input: a string representing a list of (Int,Int) pairs.
-  output: the actual list of pairs represented by the input
-          string.
----------------------------------------------------------------}
convertToList :: String -> [(Int,Int)]
convertToList s = l
  where l = read s :: [(Int,Int)]


{---------------------------------------------------------------
-  description: this function builds a string of the format
-               specified in part 2 of the assignment.
-  input: a list of (Int,Int) pairs.
-  output: a string representing the input list, which follows
-          format required in part 2 of the assignment.
----------------------------------------------------------------}
buildString :: [(Int,Int)] -> String
buildString [] = ""
buildString ((h1,h2):tail) = show h1 ++ " -- " ++ show h2 ++ "\n" ++ buildString tail




{--------------------------------------------------------------}
{-------------------------- Part III --------------------------}
{--------------------------------------------------------------}


{---------------------------------------------------------------
-  description: this function applies a comparator network
-               (represented by a list of (Int,Int) pairs to a
-               list of numbers. It does this by successively
-               comparing the elements of the list at the
-               indices provided in the current (Int,Int) pair.
-
-               if the element at the first index is greater
-               than the element at the second index, the two
-               elements are swapped.
-  input:
-    1. a list of (Int,Int) pairs representing a comparator
-       network.
-    2. a list of numbers which will be ordered based on the
-       provided comparator network.
-  output: the list of numbers provided, but re-ordered based on
-          the provided comparator network.
----------------------------------------------------------------}
applyComparator :: [(Int,Int)] -> [Int] -> [Int]
applyComparator [] s = s
applyComparator ((h1,h2):tail) s =
  if s!!(h1-1) > s!!(h2-1)
    then (applyComparator tail (swap s h1 h2))
  else (applyComparator tail s)


{---------------------------------------------------------------
-  description: this function takes a list of numbers and two
-               (integer) indices, and swaps the elements of the
-               list at those two indices.
-  input:
-    1. a list of numbers containing two elements to be swapped.
-    2. the index of the first element to be swapped.
-    3. the index of the second element to be swapped.
-  output: the input list except that the elements at the two
-          indices provided are swapped.
----------------------------------------------------------------}
swap :: [Int] -> Int -> Int -> [Int]
swap l ind1 ind2 = l1 ++ [l!!(i2)] ++ l2 ++ [l!!(i1)] ++ l3
  where i1 = ind1 - 1
        i2 = ind2 - 1
        l1 = take i1 l
        l2 = drop ind1 (take i2 l)
        l3 = drop ind2 l




{-------------------------------------------------------------}
{-------------------------- Part IV --------------------------}
{-------------------------------------------------------------}


{---------------------------------------------------------------
-  description: this function takes a list of (Int,Int) pairs
-               representing a comparator network and puts it
-               into parallel form. It does this by recursively
-               finding the parallel elements of each pair
-               (beginning with the first one), and then
-               removing each parallel element found from the
-               list of pairs.
-  input: a list of (Int,Int) pairs representing a comparator
-         network.
-  output: a list of lists of (Int,Int) pairs representing the
-          input comparator network, but in the parallel form
-          described in part four of the assignment.
----------------------------------------------------------------}
parallelize :: [(Int,Int)] -> [[(Int,Int)]]
parallelize [] = []
parallelize (x:xs) = (findParallels [x] xs):(parallelize xss)
  where xss = removeMultiple (findParallels [x] xs) (x:xs)


{---------------------------------------------------------------
-  description: given a list of parallel (Int,Int) pairs and a
-               list of (Int,Int) pairs to search, this function
-               identifies and adds all parallel pairs remaining
-               in the search list to the parallel list.
-  input:
-    1. a list of parallel (Int,Int) pairs.
-    2. a list of (Int,Int) pairs which will be searched for any
-       pairs that can be added to the parallel list.
-  output: the list of (Int,Int) pairs containing all pairs
-          which are parallel to each element in the input
-          parallel list.
----------------------------------------------------------------}
findParallels :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findParallels a [] = a
findParallels a (b:bs) =
  if (isParallel b a)
    then (findParallels (a++[b]) bs)
  else (findParallels a bs)


{---------------------------------------------------------------
-  description: this function determines whether or not an
-               (Int,Int) pair is parallel to a list of parallel
-               (Int,Int) pairs.
-  input:
-    1. an (Int,Int) pair which may or may not be parallel to
-       input (2)
-    2. a list of (Int,Int) pairs which are all parallel to each
-       other.
-  output: the boolean value indicating whether or not the input
-          pair is parallel to each element in the input list.
----------------------------------------------------------------}
isParallel :: (Int,Int) -> [(Int,Int)] -> Bool
isParallel _ [] = True
isParallel (x,y) ((z1,z2):zs) =
  if x /= z1 && y /= z1 && x /= z2 && y /= z2
    then (isParallel (x,y) zs)
  else False


{---------------------------------------------------------------
-  description: this function removes a single (Int,Int) pair
-               from a list of (Int,Int) pairs.
-  input:
-    1. an (Int,Int) pair which will be removed from input (2)
-    2. a list of (Int,Int) pairs which will have input (1)
-       removed from it.
-  output: the input list except that the input pair will be
-          removed.
----------------------------------------------------------------}
removeFromList :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
removeFromList x [] = []
removeFromList x (y:[]) | (x == y) = []
                        | otherwise = y:[]
removeFromList x (y:ys) | (x == y) = ys
                        | otherwise = y:(removeFromList x  ys)


{---------------------------------------------------------------
-  description: this function removes the first instance of
-               (possibly) several (Int,Int) pairs from a list
-               of (Int,Int) pairs.
-  input:
-    1. a list of (Int,Int) pairs containing all of the pairs
-       which should be removed from input (2)
-    2. a list of (Int,Int) pairs which should have the pairs of
-       input (1) removed from it.
-  output: the original input (2), except with the first
-          instances of each pair in input (1) removed from it.
----------------------------------------------------------------}
removeMultiple :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
removeMultiple _ [] = []
removeMultiple [] (y:ys) = (y:ys)
removeMultiple (x:xs) (y:ys) = removeMultiple xs (removeFromList x (y:ys))


{---------------------------------------------------------------
-  description: this function takes a comparator network in
-               parallel form and builds a string representation
-               of it using the buildParallelStringInner helper
-               function.
-  input: a list of lists of (Int,Int) pairs, representing a
-         comparator network in parallel form.
-  output: the string representation of the input, as described
-          in part four of the assignment.
----------------------------------------------------------------}
buildParallelString :: [[(Int,Int)]] -> String
buildParallelString [] = ""
buildParallelString (x:xs) = (buildParallelStringInner x)++(buildParallelString xs)


{---------------------------------------------------------------
-  description: this function takes a list of parallel (Int,Int)
-               pairs and constructs a string representation
-               of that list, as described in part four of the
-               assignment.
-  input: a list of parallel (Int,Int) pairs.
-  output: the string representation of the input list.
----------------------------------------------------------------}
buildParallelStringInner :: [(Int,Int)] -> String
buildParallelStringInner [] = ""
buildParallelStringInner ((h1,h2):[]) = show h1 ++ " -- " ++ show h2 ++ "\n"
buildParallelStringInner ((h1,h2):tail) = show h1 ++ " -- " ++ show h2 ++ " , " ++ buildParallelStringInner tail




{------------------------------------------------------------}
{-------------------------- Part V --------------------------}
{------------------------------------------------------------}


{---------------------------------------------------------------
-  description: this function generates all possible
-               combinations of zeros and ones of a designated
-               length.
-  input: an integer representing the length of the sequences to
-         be generated.
-  output: a list of lists of zeros and ones, where each nested
-          list is a combination of zeros and ones and the outer
-          list contains all possible combinations of the given
-          length.
----------------------------------------------------------------}
create01 :: Int -> [[Int]]
create01 0 = [[]]
create01 x = zeros++ones
  where zeros = map (0:) (create01 (x-1))
        ones = map (1:)  (create01 (x-1))


{---------------------------------------------------------------
-  description: this function utilizes the zero-one principle
-               to determine whether a given comparator network
-               is a sorting network.
-  input:
-    1. a list of (Int,Int) pairs representing a comparator
-       network.
-    2. a list containing all possible combinations of zeros
-       and ones of length n, where n is the number of wires in
-       the given comparator network.
-  output: a boolean value indicating whether or not the given
-          comparator network is a sorting network.
----------------------------------------------------------------}
isSortingNetwork :: [(Int,Int)] -> [[Int]] -> Bool
isSortingNetwori [] _ = False
isSortingNetwork _ [] = True
isSortingNetwork n (x:xs) =
  if (isSorted (applyComparator n x)) then (isSortingNetwork n xs)
  else False


{---------------------------------------------------------------
-  description: this function determines whether or not a list
-               of numbers is sorted in ascending order.
-  input: a list of numbers, which may or may not be sorted.
-  output: the boolean value indicating whether or not the given
-          list is sorted in ascending order.
----------------------------------------------------------------}
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x1:x2:xs) =
  if x1 <= x2 then isSorted (x2:xs)
  else False


{---------------------------------------------------------------
-  description: this function determines the number of wires in
-               a comparator network.
-  input:
-    1. a list of (Int,Int) pairs representing a comparator
-       network.
-    2. the maximum value found within the list so far (the
-       number of wires found so far).
-  output: the number of wires in the given comparator network.
----------------------------------------------------------------}
numWires :: [(Int,Int)] -> Int -> Int
numWires [] max = max
numWires ((x1,x2):xs) max | (x1 > max) = if (x1 > x2) then (numWires xs x1) else (numWires xs x2)
                          | (x2 > max) = numWires xs x2
                          | otherwise = numWires xs max




{-------------------------------------------------------------}
{-------------------------- Part VI --------------------------}
{-------------------------------------------------------------}


{---------------------------------------------------------------
-  description: this function creates a sorting network, based
-               on insertion sort, with a specified number of
-               wires.
-  input: an integer representing the number of wires that the
-         sorting network produced should have.
-  output: the sorting network based on insertion sort which
-          has n wires, where n is specified by the input.
----------------------------------------------------------------}
createNetwork :: Int -> [[(Int,Int)]]
createNetwork 0 = []
createNetwork 1 = []
createNetwork x = left ++ middle ++ right
  where middle = [createPillar x]
        right = buildRight (x-1)
        left = buildLeft right


{---------------------------------------------------------------
-  description: this function creates a single "pillar", or list
-               of parallel pairs, within a sorting network
-               based on insertion sort.
-
-               for example: for the sorting network based on
-                            insertion sort with five wires has
-                            the folllowing pillars:
-                              -  [(1,2)]
-                              -  [(2,3)]
-                              -  [(1,2),(3,4)]
-                              -  [(2,3),(4,5)]
-                              -  [(1,2),(3,4)]
-                              -  [(2,3)]
-                              -  [(1,2)]
-  input: an integer representing what the maximum value of this
-         pillar is.
-  output: the pillar specified by the input.
----------------------------------------------------------------}
createPillar :: Int -> [(Int,Int)]
createPillar 0 = []
createPillar 1 = []
createPillar x = (createPillar (x-2))++[(x-1,x)]


{---------------------------------------------------------------
-  description: this function builds the right side of a sorting
-               network based on insertion sort (everything to
-               the right of the middle pillar).
-  input: an integer representing the maximum value of the
-         pillar which we are currently on.
-  output: the right side of the sorting network based on
-          insertion sort, which has x+1 wires, where x is the
-          input of the root call to this recursive function.
----------------------------------------------------------------}
buildRight :: Int -> [[(Int,Int)]]
buildRight 0 = []
buildRight 1 = []
buildRight x = (createPillar (x)):(createPillar (x-1)):(buildRight (x-2))


{---------------------------------------------------------------
-  description: this function builds the left side of a sorting
-               network based on insertion sort (everything to
-               the left of the middle pillar) by reversing the
-               right side.
-  input: a list of lists of (Int,Int) pairs which represents
-         the right side of a sorting network based on insertion
-         sort.
-  output: the left side of the sorting network based on
-          insertion sort corresponding to the input.
----------------------------------------------------------------}
buildLeft :: [[(Int,Int)]] -> [[(Int,Int)]]
buildLeft [] = []
buildLeft r = reverse r
