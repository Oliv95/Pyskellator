module Main where 
import Language.Python.Common
import Language.Obfuscator
import System.Environment

main :: IO()
main = do
    -- Mer av detta borde ligga i obfuscator känns det som
    args <- getArgs
    let inFile = head args
    let outFile = head (tail args)
    source <- readFile inFile
    let stms = stmListFromSource source
    obfuAst  <- mapM transformStatement stms
    --pretty känns rätt sketchy
    let code = unlines (fmap show (fmap pretty obfuAst))
    writeFile outFile code
    


