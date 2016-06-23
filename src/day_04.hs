import System.Environment
import Crypto.Hash
import Data.ByteString.Char8 (ByteString, pack)

md5str :: ByteString -> String
md5str bs = show (hash bs :: Digest MD5)

test :: String -> Int -> String
test secret nonce = md5str . pack $ secret ++ show nonce

check :: Int -> String -> Bool
check target hsh = concat (replicate target "0") == take target hsh

mine :: String -> Int -> Int -> Int
mine secret target nonce = if check target (test secret nonce)
                           then nonce
                           else mine secret target $ succ nonce

main = do (secret:_) <- getArgs
          putStrLn "First nonce which hashes below difficulty 5:"
          print $ mine secret 5 0
          putStrLn "First nonce which hashes below difficulty 6:"
          print $ mine secret 6 0
