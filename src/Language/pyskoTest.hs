import Language.Python.Common
import Language.Python.Version3.Parser

unModule :: ModuleSpan -> [Statement SrcSpan] 
unModule (Module xs) = xs

-- Gets the SrcSpan list from python code
f a = unModule $ fst $ (\(Right x) -> x) (parseModule a "")
fb = fst $ (\(Right x) -> x) (parseModule a "")

a = "x = 5\ny=8"

changeAssignment (Assign x expr c) = Assign x (exprToLambda expr) c
changeAssignment              a = a

doIt a = pretty (Module (fmap changeAssignment (f a)))

main :: IO()
main = undefined

exprToLambda e@(Int value lit a) = Call (Paren (Lambda [] e a) a) [] a
exprToLambda e                   = e

splitAssign (Assign t e a) = (t,e)

idLambda = Lambda [Param  (Ident "x" "") Nothing Nothing ""] (Var (Ident "x" "") "") ""






fromRight (Right a) = a
