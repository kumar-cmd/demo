import Data.List
import System.IO
-- import Data.Set

-- main = do
-- 	putStrLn "hello world"

addme :: Float -> Float -> Float -> Float

addme x y z = x + y + z

f :: Int -> Int

f x = x ^ 2

f1 :: Int -> Int -> Int 
f1 x y = f x + f y

f2 :: Int -> Int -> Bool; 
f2 a b = a == b

v = [-4,-2..44]

y = sort (nub [1,1,1,2,3,3,4,5]) == sort (nub [3,2,2,2,1,5,4,5])

z1 = nub [3,2,2,2,1,5,4,5]
-- z2 = fromList [3,2,2,2,1,5,4,5]


powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:s | s <- powerset xs] ++ powerset xs


div2 :: [Int]
div2 = take 100 [x | x <- [1..], x `mod` 2 == 0]
div3 :: [Int]
div3 = take 100 [x | x <- [1..], x `mod` 3 == 0]

union2 :: [Int] -> [Int] -> [Int]
union2 setA setB = [x | x <- [1..], x `elem` setA || x `elem` setB]


intersection2 :: [Int] -> [Int] -> [Int]
intersection2 setA setB = [x | x <- [1..], x `elem` setA && x `elem` setB]


-- The difference of two sets A and B, A − B = {x | x ∈ A and x ∈/ B}.
difference2 :: [Int] -> [Int] -> [Int]
difference2 setA setB = [x | x <- [1..], x `elem` setA && x `notElem` setB]

-- The complement of a set A, A′ = {x | x ∈/ A}.7 
complement2 :: [Int] -> [Int]
complement2 setA = [x | x <- [1..], x `notElem` setA]


doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 +1

g=['w','o'] ++ ['o','t'] 

main = do
    putStrLn "What's your name: "
    name <- getLine
    putStrLn ("Hello " ++ name)  

times4 :: Int -> Int
times4 x = x * 4
multBy4 :: [Int] -> [Int]
multBy4 [] = []
 
-- Takes the 1st value off the list x, multiplies it by 4 and stores it in the 
-- new list
-- xs is then passed back into multBy4 until there is nothing left of the list -- to process (Recursion)
multBy4 (x:xs) = times4 x : multBy4 xs

ss :: [Int] -> Int
ss xs = sum [ x*x | x <- xs, x > 0 ]





l :: [Char] -> [[(Char, Char)]]
l [] = [[]]
l (x:xs) = [(x,n):e | n <- ns , e <- [[('a','F')],[('a','T')]]]
        where ns = ['F','T']











