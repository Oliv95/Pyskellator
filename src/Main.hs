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
    let stms = stmListFromSource source
    obfuAst  <- mapM transformStatement stms
    let code = unlines (fmap show (fmap pretty obfuAst))
    writeFile outFile code
    


