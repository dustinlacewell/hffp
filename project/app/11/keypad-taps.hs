import Data.Char
import Data.List
import Data.Maybe
import Data.Function

main = print True

newtype Keypad = Keypad [String]

defaultPad = Keypad [
        "1", "abc2",  "def3",
     "ghi4", "jkl5",  "mno6",
    "pqrs7", "tuv8", "wxyz9",
       "^*",  "+_0",   ".,#"]

-- | Replace capital characters with * and lowered character
-- >> normalizeMessage "Hello World"
-- "*hello *world"
normalizeMessage :: String -> String
normalizeMessage =
    foldr f ""
    where f c s = if isUpper c then
                    ['*', toLower c] ++ s
                  else c:s

-- | Return keyIndex, charIndex for char
-- >>> pressFor defaultPad 'g'
-- Just (3,1)
-- >>> pressFor defaultPad 'z'
-- Just (8,4)
-- >>> pressFor defaultPad  '%'
-- Nothing
-- >>> pressFor defaultPad '5'
-- Just (4,4)
pressFor :: Keypad -> Char -> Maybe (Int, Int)
pressFor (Keypad keys) c =
    case findIndex (elem c) keys of
        Nothing -> Nothing
        Just keyIndex ->
            let set = keys !! keyIndex
            in case elemIndex c set of
                Nothing -> Nothing
                Just charIndex -> Just (keyIndex, charIndex + 1)

-- | Convert a key and presses to char
-- >>> pressesToKey defaultPad (3, 1)
-- 'd'
-- >>> pressesToKey defaultPad (9, 4)
-- 'z'
-- >>> pressesToKey defaultPad (5, 4)
-- '5'
pressesToKey :: Keypad -> (Int,  Int) -> Char
pressesToKey (Keypad phone) (keyIndex, presses) =
    let key = phone !! (keyIndex - 1)
    in key !! (presses - 1 `mod` length key)

-- | Generate a sequence of keys to press
-- >>> seqForPress defaultPad (3, 1)
-- "4"
-- >>> seqForPress defaultPad (9, 4)
-- "****"
seqForPress :: Keypad -> (Int, Int) -> String
seqForPress (Keypad phone) (keyIndex, count) =
    let keyStr = phone !! keyIndex
        key = keyStr !! (length keyStr - 1)
        ret = replicate count key
    in ret

-- | Convert message to series of keypad presses
-- >>> convertKeypadMessage defaultPad "Hello World"
-- "**4433555555666**96667775553"
convertKeypadMessage :: Keypad -> String -> String
convertKeypadMessage phone msg =
    mapMaybe (pressFor phone) (normalizeMessage msg)
    & concatMap (seqForPress phone)

