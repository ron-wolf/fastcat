import System.Environment (getArgs)
import Control.Monad      ( (>=>) )
main = getArgs >>= traverse (readFile >=> putStrLn)
