import Data.Function
import Data.List

main = print True

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
-- | True if all elements of first arg appear in second arg
-- >>> isSubseqOf "blah" "blahwoot"
-- True
-- >>> isSubseqOf "blah" "wootblah"
-- True
-- >>> isSubseqOf "blah" "wboloath"
-- True
-- >>> isSubseqOf "blah" "wootbla"
-- False
-- >>> isSubseqOf "blah" "halbwoot"
-- False
-- >>> isSubseqOf "blah" "blawhoot"
-- True
isSubseqOf [] [] = True
isSubseqOf _ [] = True
isSubseqOf [] _ = True
isSubseqOf (x:xs) ys =
    case elemIndex x ys of
        Just index ->
            Prelude.drop index ys
            & isSubseqOf xs
        Nothing -> False
