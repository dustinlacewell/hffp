import Data.Char
import Data.Sequence ( mapWithIndex )

type Key = String

rotate :: Int -> Char -> Char
rotate n c = chr $ ord c + n

nForChar :: Char -> Int
nForChar c = ord c - ord 'a'

mapWithIdx f = zipWith f [0..]

encrypt :: Key -> String -> String
encrypt k =
    mapWithIdx (\i c ->
        let j = k !! (i `mod` keylen)
            n = nForChar j
        in rotate n c)
    where keylen = Prelude.length k

decrypt :: Key -> String -> String
decrypt k =
    mapWithIdx (\i c ->
        let j =  k !! (i `mod` keylen)
            n = negate (nForChar j)
        in rotate n c)
    where keylen = Prelude.length k

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

