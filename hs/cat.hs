import           System.Environment (getArgs)
main = putStrLn =<< readFile =<< (head <$> getArgs)
