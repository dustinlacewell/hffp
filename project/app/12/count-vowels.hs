import Data.Maybe
import Text.Regex

main = print True

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiouAEIOU"

getVowels :: String -> String
getVowels = filter isVowel

-- | Count the cowels in a string
-- >>>  countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Int
countVowels = length . getVowels
