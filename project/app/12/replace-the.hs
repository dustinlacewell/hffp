import Text.Regex
import Data.Maybe
import Data.Function
import Data.ByteString

main = print True

-- | Return whether string is "the" or variation
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s =
    let p = mkRegex "^[tT][hH][eE]$"
    in case matchRegex p s of
        Nothing -> Just s
        Just o -> Nothing

-- | Replace all occurances of `the` with `a`
-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe s =
    unwords $ Prelude.map (fromMaybe "a" . notThe) (words s)
