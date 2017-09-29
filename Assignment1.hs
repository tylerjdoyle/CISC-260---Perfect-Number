module Assignment1 where

{- Assignment 1 code by Tyler Doyle (10129777)

This code has two main functions, one to test whether or not a number is a perfect
number and another to find the nth number of a certain property. -}

{- This function takes an int as a parameter and tests whether or not it is a 
perfect number. If it is it returns True, otherwise False. -}
isPerfect :: Int -> Bool
isPerfect num
 | num < 1 = False
 | 2 * num == perfectNumberHelper num num = True
 | otherwise = False
 
{- This function is a helper function to isPerfect to see if a number's factors
sum to 2 times the number (is a perfect number). -}
perfectNumberHelper :: Int -> Int -> Int
perfectNumberHelper num 1 = 1
perfectNumberHelper num factor
 | mod num factor == 0 = factor + perfectNumberHelper num (factor - 1)
 | otherwise = perfectNumberHelper num (factor - 1)
 
{- This function takes a function and an int as parameters and finds the nth number 
that the given function returns true for. -}
nthTrue :: (Int -> Bool) -> Int -> Int
nthTrue function num
 | num < 1 = error "nthTrue called with n < 1"
 | otherwise = nthTrueHelper function num 1
 
{- This function is a helper function for nthTrue which finds the nth number that
fits some property. -}
nthTrueHelper :: (Int -> Bool) -> Int -> Int -> Int
nthTrueHelper function numProp num
 | numProp == 0 = (num - 1)
 | function num == True = nthTrueHelper function (numProp - 1) (num + 1)
 | otherwise = nthTrueHelper function numProp (num + 1)