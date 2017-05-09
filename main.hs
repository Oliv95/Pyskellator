import System.Environment
import Language.Obfuscator

main = do
    args <- getArgs
    inFile <- head args
    outFile <- head $ tail args
    let stms <- stmListFromSource inFile (takeWhile (\x -> /= '.') inFile)
    obfu <- mapM transformStatement stms
    let obfuCode = unlines (fmap show (fmap pretty obfu)
    writeFile outFile
