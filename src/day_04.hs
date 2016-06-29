import           Crypto.Hash
import           Data.ByteString.Char8 (ByteString, pack)
import           System.Environment
import           Welcome

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

usage :: IO ()
usage = putStrLn "Usage: day_04.exe secret_key"

soln :: [String] -> IO ()
soln args = do let secret = head args
               welcome 4
               putStrLn "First nonce which hashes below difficulty 5:"
               print $ mine secret 5 0
               putStrLn "First nonce which hashes below difficulty 6:"
               print $ mine secret 6 0

main = do args <- getArgs
          if null args then usage
          else soln args
