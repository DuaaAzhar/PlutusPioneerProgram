module Main (main) where


import Week06.Utils (unsafeTokenNameToHex)
import System.Environment (getArgs)
import Data.String    (IsString (..))



main :: IO()
main = do
    [tn'] <- getArgs
    let tn = fromString tn'
    putStrLn $ unsafeTokenNameToHex tn
