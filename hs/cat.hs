import           System.Environment (getArgs)
import           Control.Monad      ((<=<))
main = traverse (putStrLn <=< readFile) =<< getArgs
