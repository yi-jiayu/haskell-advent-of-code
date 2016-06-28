import System.Environment

prompt :: [String] -> IO ()
prompt [] = putStrLn "Please provide some arguments"
prompt (x:_) = putStrLn ("Your first argument was: " ++ x)

main = do args <- getArgs
          prompt args
