-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo


reverse :: [a] -> [a]
reverse xs = reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' [] result = result
reverse' (x:xs) result = reverse' xs (x:result)

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = sumHelper (reverse xs) ys

sumHelper :: [a] -> [a] -> [a]
sumHelper [] ys = ys
sumHelper (x:xs) ys = sumHelper xs (x:ys)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (x:xs) = xs

length0 :: [a] -> Int
length0 xs = length' xs 0

length' :: [a] -> Int -> Int
length' [] i = i
length' (x:xs) i = length' xs (i+1)

map0 :: (a -> b) -> [a] -> [b]
map0 pred xs = map' pred (reverse xs) []

map' f [] result = result
map' f (x:xs) result = map' f xs ((f x) : result)

filter0 :: (a -> Bool) -> [a] -> [a]
filter0 pred xs = filter' pred (reverse xs) []

filter' pred [] result = result
filter' pred (x:xs) result
  | pred x == True = filter' pred xs (x:result)
  | otherwise = filter' pred xs result

concat :: [[a]] -> [a]
concat xs = concat' (reverse xs) []

concat' :: [[a]] -> [a] -> [a]
concat' [] result = result
concat' (x:xs) result = concat' xs (x +++ result)

(!!!) :: [a] -> Int -> a
(!!!) xs 0 = head xs
(!!!) xs i = (!!!) (tail xs) (i - 1)

foldlInt :: (Int -> Int -> Int) -> [Int] -> Int
foldlInt f [] = 0
foldlInt f (x:[]) = x
foldlInt f (x:xs) = fold' f xs x 

fold' :: (a -> a -> a) -> [a] -> a -> a
fold' f [] result = result
fold' f (x:xs) result = fold' f xs (f result x)

repeat :: a -> Int -> [a]
repeat x i = repeat' x i []

repeat' :: a -> Int -> [a] -> [a]
repeat' x 0 result = result
repeat' x i result = repeat' x (i - 1) (x:result)

take0 :: Int -> [a] -> [a]
take0 i xs = take' i xs []

take' :: Int -> [a] -> [a] -> [a]
take' 0 xs result = result
take' i (x:xs) result = take' (i - 1) xs (result +++ [x])

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

buildList :: Int -> Int -> Int -> [Int]
buildList start count end = (repeat start count) +++ [end]

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums i = sums' [] 1 i

sums' :: [Int] -> Int -> Int -> [Int]
sums' result counter i
  | counter > i = reverse result 
  | otherwise = sums' ((sum counter) : result) (counter + 1) i

sum :: Int -> Int
sum x = sum' x 1 0

sum' :: Int -> Int -> Int -> Int
sum' x counter result
  | x < counter = result
  | otherwise = sum' x (counter + 1) (result + counter)


------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast def xs = head (reverse xs)

------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- This time, implement indexDefault using pattern matching and
-- recursion.
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault xs i def
  | (length0 xs) <= i = def
  | otherwise = xs !!! i

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.

sorted :: [Int] -> Bool
sorted xs = sorted' xs True

sorted' :: [Int] -> Bool -> Bool
sorted' [] result = result
sorted' (x:[]) result = result
sorted' (x:xs) result = sorted' xs (result && (x <= (head xs)))

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])

sumsOf :: [Int] -> [Int]
sumsOf xs = sumsOf' (reverse xs) []

sumsOf' :: [Int] -> [Int] -> [Int]
sumsOf' [] result = result
sumsOf' (x:xs) result =  sumsOf' xs ((foldlInt (+) (x:xs)) : result)

------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge xs ys = merge' xs ys []

merge' :: [Int] -> [Int] -> [Int] -> [Int]
merge' [] [] result = result
merge' [] ys result = result +++ ys
merge' xs [] result = result +++ xs
merge' (x:xs) (y:ys) result
  | x < y = merge' xs (y:ys) (result +++ [x])
  | otherwise = merge' (x:xs) ys (result +++ [y])



------------------------------------------------------------------------------
-- Ex 8: define the function mymaximum that takes a list and a
-- function bigger :: a -> a -> Bool and returns the
-- biggest of the list, according to the comparing function.
--
-- An initial biggest value is provided to give you something to
-- return for empty lists.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\_ _ -> True) 4 [1,3,2]
--     ==> 2  -- new value is always the biggest, so returns the last

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum bigger initial [] = initial
mymaximum bigger initial (x:xs)
  | bigger x initial = mymaximum bigger x xs
  | otherwise = mymaximum bigger initial xs

------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs
  | length0 as > length0 bs = map2' f (take0 (length0 bs) as) bs []
  | otherwise = map2' f as (take0 (length0 as) bs) []

map2' :: (a -> b -> c) -> [a] -> [b] -> [c] -> [c]
map2' f [] [] result = result
map2' f (x:xs) (y:ys) result = map2' f xs ys (result +++ [(f x y)])


------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f xs = justGetTheFuckingValues (map0 f xs) []

-- dude just implement maybemap without maybe
-- shittily implemented restrictions 0/5

justGetTheFuckingValues :: [Maybe a] -> [a] -> [a]
justGetTheFuckingValues [] result = result
justGetTheFuckingValues (Nothing:xs) result = justGetTheFuckingValues xs result
justGetTheFuckingValues (Just x : xs) result = justGetTheFuckingValues xs (result +++ [x])