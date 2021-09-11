import Data.Maybe
import Text.Regex
import Data.Function
import Data.Char


isThe :: String -> Bool
isThe s =
    let p = mkRegex "^[tT][hH][eE]$"
    in case matchRegex p s of
        Nothing -> False
        Just o -> True

foldl2 :: (b -> a -> a -> b) -> b -> [a] -> b
foldl2 _ b [] = b; foldl2 _ b [_] = b; foldl2 f b (a:a2:as) =
    foldl2 f b' (a2:as)
    where b' = f b a a2

isVowel :: [Char] -> Bool
isVowel [] = False
isVowel [x] = x `elem` "aeiouAEIOU"
isVowel (x:xs) = x `elem` "aeiouAEIOU"

countTheBeforeVowel :: String -> Int
countTheBeforeVowel msg = foldl2
    (\b w1 w2 -> if isThe w1 && isVowel w2 then b + 1 else b) 0 (words msg)

map2 :: (a -> a -> a) -> [a] -> [a]
map2 f [] = []
map2 f [a] = [a]
map2 f (a:b:cs) =
    a' : map2 f xs
    where a' = f a b
          xs = b:cs


isTheAndVowel :: String -> String -> Bool
isTheAndVowel w1 w2
    | w1 == "the" && head w2 `elem` "aeiou" = True
    | otherwise = False

theToA :: String -> String
theToA s =
    let theToA' =
            \w1 w2 ->
                if isTheAndVowel w1 w2 then "a" else w1
        ws = map2 theToA' (words s)
    in unwords ws


main :: IO ()
main = do
    let s = "the ape ate the grass??"
    print s
    print $ map2 (\c1 c2 -> if isAlpha c1 && c2 `elem` "aeiou" then '!' else c1) s
    print $ words s
    print $ isTheAndVowel "the" "cow"
    print $ isTheAndVowel "the" "age"
    print $ map2 (\w1 w2 -> if isTheAndVowel w1 w2 then "a" else w1) (words s)
    print $ theToA "the cow ate the grass"
