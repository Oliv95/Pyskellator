module Main where 
import Language.Python.Common
import Language.Obfuscator
import System.Environment

main :: IO()
main = do
    args <- getArgs
    let inFile = head args
    let outFile = head (tail args)
    source <- readFile inFile
    code <- obfuscate source
    writeFile outFile code
    


