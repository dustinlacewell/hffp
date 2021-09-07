import Data.Char
import Data.Sequence ( mapWithIndex )

type Key = String

rotate :: Int -> Char -> Char
rotate n c = chr $ ord c + n

nForChar :: Char -> Int
nForChar c = ord c - ord 'a'

mapWithIdx f = zipWith f [0..]

crypt :: (Char -> Int) -> Key -> String -> String
crypt nf k =
    mapWithIdx (\i c ->
        let j = k !! (i `mod` length k)
            n = nf j
        in rotate n c)

encrypt :: Key -> String -> String
encrypt = crypt nForChar

decrypt :: Key -> String -> String
decrypt = crypt (negate . nForChar)

main :: IO ()
main =
    let key = "foxhound"
        plaintext = "All your base are belong to us"
        ciphertext = encrypt key plaintext
        decrypted = decrypt key ciphertext
    in do
        putStrLn $ "Key: " ++ key
        putStrLn $ "Plaintext: " ++ plaintext
        putStrLn $ "Ciphertext: " ++ ciphertext
        putStrLn $ "Decrypted: " ++ decrypted

