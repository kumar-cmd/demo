import Data.Char

makeLower :: Char -> Char
makeLower ch |('A'<= ch && ch <= 'Z') =
                chr (ord ch - (ord 'A' - ord 'a'))
           | otherwise = ch


makeCapital :: Char -> Char
makeCapital ch |('a'<= ch && ch <= 'z') =
                chr (ord ch + (ord 'A' - ord 'a'))
           | otherwise = ch

convertToLower :: String -> String
convertToLower "" = ""
convertToLower (x:xs) =  (makeLower x) : (convertToLower xs)

convertToCapital :: String -> String
convertToCapital (xs) = map makeCapital xs

checkChar :: [(Char, Char)] -> Bool
checkChar [] = True
checkChar (x:xs) | makeLower (fst x) /= makeLower (snd x) = False
              | otherwise = checkChar xs
-- 1
sameString :: String -> String -> Bool
sameString str1 str2 | length str1 /= length str2 = False
                     | otherwise = checkChar (zip str1 str2)

-- 2
prefix :: String -> String -> Bool
prefix str1 str2 | length str1 > length str2 = False
                 | otherwise = checkChar (zip str1 str2)




-- 3


contains :: String -> String -> Bool
contains str1 str2 | length str1 < length str2 = False
                   | checkChar (zip str1 str2) == True = True
                   | otherwise = contains (drop 1 str1) str2



-- 4.
takeUntil :: String -> String -> String
takeUntil substr str | contains str substr = head [ take i str | i <- [0..length str], prefix substr (drop i str)]
                     | otherwise = str

dropUntil :: String -> String -> String
dropUntil substr str | contains str substr = head [ drop (i + length substr) str | i <- [0..length str], prefix substr (drop i str)]
                     | otherwise = ""


-- 5

split :: String -> String -> [String]
split str1 "" = []
split str1 str2 | str2 == "" = []
                |takeUntil str1 str2 /= "" && dropUntil str1 str2 == "" && contains str2 str1 = [takeUntil str1 str2,""]
                -- |takeUntil str1 str2 = "" && dropUntil str1 str2 == "" = [""]
                |otherwise = takeUntil str1 str2 : split str1 (dropUntil str1 str2)

type Password = String
type Locked a = Password -> Maybe a

createLock :: Password -> a -> Password -> Maybe a
createLock x y h | x == h = Just y
                 | otherwise = Nothing


                 
