module VigenereCipher where

import Data.Char
import Data.Function

type Key = String

-- | Feed the output of a function back into itself n times
-- This is a test helper function
-- >>> churn (+1) 0 5
-- 5
churn f a 0 = a
churn f a n = churn f (f a) (n - 1)

baseOrdinal = ord 'a'

-- | Compute ordinal relative to 'a'
-- >>> debaseOrdinal 'a'
-- 0
-- >>> debaseOrdinal 'z'
-- 25
debaseOrdinal c = ord c - baseOrdinal

-- | Rotate a character by a given number of places
-- >>> rotate 0 'a'
-- 'a'
-- >>> rotate 1 'a'
-- 'b'
-- >>> rotate 1 'z'
-- 'a'
rotate amount char = chr $
    debaseOrdinal char
    & (+ amount)
    & (`mod` 26)
    & (+ baseOrdinal)

-- | cycle key by 1, return tip and cycled key
-- >>> cycleKey "ally"
-- ('a',"llya")
-- >>> churn (\(c, k) -> cycleKey k) ('y', "ally") 4
-- ('y',"ally")
cycleKey key = (keyChar, newKey) where
    (keyChar:restKey) = key
    newKey = restKey ++ [keyChar]

-- | Crypt a valid character using the given key and offset function
-- >>> handleChar debaseOrdinal "az" "a" ""
-- State {key = "za", input = "", output = "a"}
handleChar offsetFun key input output =
    State key' input' output'
    where
        char : input' = input
        (keyChar, key') = cycleKey key
        offset = offsetFun keyChar
        char' = rotate offset char
        output' = output ++ [char']

-- | Skip the current character since it's not a valid character
-- >>> skipChar "ab" " b" "a"
-- State {key = "ab", input = "b", output = "a "}
skipChar key input output =
    State key rest $ output ++ [char]
    where char:rest = input

data State = State {
    key :: Key,
    input :: String,
    output :: String
} deriving (Show)

-- | Encrypt a single character in the State
-- >>> worker debaseOrdinal (State "a" "b" "")
-- State {key = "a", input = "", output = "b"}
-- >>> worker debaseOrdinal (State "a" " b" "a")
-- State {key = "a", input = "b", output = "a "}
worker offsetFun (State key input output) =
    if isAlpha $ head input then
        handleChar offsetFun key input output
    else
        skipChar key input output

-- | Process input until it's empty
work f s@(State key input output)
    | input == "" = s
    | otherwise = work f $ f s

-- | Crypt a string using the given key
crypt processor key input =
    work processor (State key input "") & output

-- | Encrypt a string using the given key
-- >>> encrypt "ally" "meet at dawn"
-- "mppr ae oywy"
encrypt = crypt (worker debaseOrdinal)

-- | Decrypt a string using the given key
-- >>> decrypt "ally" "mppr ae oywy"
-- "meet at dawn"
decrypt = crypt $ worker (negate . debaseOrdinal)

main =
    let key = "ally"
        plaintext = "meet at dawn"
        ciphertext = encrypt key plaintext
        decrypted = decrypt key ciphertext
    in do
        putStrLn $ "Key: " ++ key
        putStrLn $ "Plaintext: " ++ plaintext
        putStrLn $ "Ciphertext: " ++ ciphertext
        putStrLn $ "Decrypted: " ++ decrypted