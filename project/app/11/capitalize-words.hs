import Data.Char
import Data.List

main = print True

-- | Capitalize a word
-- >>> capitalizeWord "Chortle"
-- "Chortle"
-- >>> capitalizeWord "chortle"
-- "Chortle"
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
-- | Split a string into words and map it to a tuple containing
-- | the original and capitalized version
-- >>> capitalizeWords "hello world"
-- [("hello","Hello"),("world","World")]
capitalizeWords "" = []
capitalizeWords s =
    let
        f :: String -> (String, String)
        f [] = ("","")
        f w@(x:xs) = (w, toUpper x : xs)
    in map f (words s)

capitalizeParagraph' :: Bool -> Char -> (Bool, Char)
capitalizeParagraph' _ '.' = (True, '.')
capitalizeParagraph' b c
    | isAlpha c && b = (False, toUpper c)
    | otherwise = (b, c)

-- | Capitalize paragraphs in a string
-- >>> capitalizeParagraph "blah. woot ha. this work?"
-- "Blah. Woot ha. This work?"
capitalizeParagraph :: String -> String
capitalizeParagraph = snd . mapAccumL capitalizeParagraph' True
