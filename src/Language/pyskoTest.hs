import Language.Python.Common
import Language.Python.Version3.Parser

unModule :: ModuleSpan -> [Statement SrcSpan] 
unModule (Module xs) = xs

-- Gets the SrcSpan list from python code
f = unModule $ fst $ (\(Right x) -> x) (parseModule a "")

a = "x = \"Hello\""

changeAssignment (Assign x _ c) = Assign x (Int 5 "5" c ) c
changeAssignment              a = a

doIt = pretty (Module (fmap changeAssignment f))

main :: IO()
main = undefined
