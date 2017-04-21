import Language.Python.Common
import Language.Python.Version3.Parser
import qualified Data.Map as Map

stmListFromSource :: String -> String -> ModuleSpan
stmListFromSource scoure fileName = fst $ (\(Right x) -> x) (parseModule scoure fileName)

unModule :: ModuleSpan -> [Statement SrcSpan] 
unModule (Module xs) = xs

todo = error "Not implemented"

transformStatement :: Statement t -> Statement t
transformStatement (Import imorts annot)                   = todo
transformStatement (FromImport from imports annot)         = todo
transformStatement (While expr body whileElse annot)       = todo
transformStatement (For vars generator body forElse annot) = todo
transformStatement (Fun name params resAnnot body annot)   = todo
transformStatement (Class name args body annot)            = todo
transformStatement (Conditional condGuards condElse annot) = todo
transformStatement (Assign target expr annot)              = todo
transformStatement (AugmentedAssign target op expr annot)  = todo
transformStatement (Decorated decorators definition annot) = todo
transformStatement (Return expr annot)                     = todo
transformStatement (Try try except tryElse finally annot)  = todo
transformStatement (Raise expr annot)                      = todo
transformStatement (With context body annot)               = todo
transformStatement (Pass annot)                            = todo
transformStatement (Break annot)                           = todo
transformStatement (Continue annot)                        = todo
transformStatement (Delete exprs annot)                    = todo
transformStatement (StmtExpr expr annot)                   = (StmtExpr (transformExpression expr) annot)
transformStatement (Global vars annot)                     = todo
transformStatement (NonLocal vars annot)                   = todo
transformStatement (Assert exprs annot)                    = todo

transformExpression :: Expr t -> Expr t
transformExpression var@(Var ident annot) = Paren (Call lambda lambdaArgs annot) annot
    where args = [Param ident Nothing Nothing annot]
          body = var
          lambda = Paren (Lambda args body annot) annot
          lambdaArgs = [ArgExpr var annot]
transformExpression e@(Int value lit annot)                      = Paren (Call lambda [] annot) annot
    where lambda = Paren (Lambda [] e annot) annot
transformExpression (Float value lit annot)                      = todo
transformExpression (Imaginary value lit annot)                  = todo
transformExpression (Bool True annot)                            = todo
transformExpression (Bool False annot)                           = todo
transformExpression (None annot)                                 = todo
transformExpression (Ellipsis annot)                             = todo
transformExpression (ByteStrings strings annot)                  = todo
transformExpression (Strings strings annot)                      = todo
transformExpression (Call expr args annot)                       = todo
transformExpression (Subscript target index annot)               = todo
transformExpression (SlicedExpr traget slices annot)             = todo
transformExpression (CondExpr trueBranch cond falseBranch annot) = todo
transformExpression (BinaryOp op left right annot)               = Call lambda [] annot
    where leftExpr = Paren (transformExpression left) annot 
          rightExpr = Paren (transformExpression right) annot
          operation = Paren (BinaryOp op leftExpr rightExpr annot) annot
          lambda    = Paren (Lambda [] (Paren operation annot) annot) annot
transformExpression (UnaryOp op expr annot)                      = (Paren (UnaryOp op (transformExpression expr) annot) annot)
transformExpression (Dot expr attribute annot)                   = todo
transformExpression (Lambda params body annot)                   = todo
transformExpression (Tuple exprs annot)                          = todo
transformExpression (Yield arg annot)                            = todo
transformExpression (Generator comprehension  annot)             = todo
transformExpression (ListComp  comprehension annot)              = todo
transformExpression (List list annot)                            = todo
transformExpression (Dictionary mappings annot)                  = todo
transformExpression (DictComp comprehension annot)               = todo
transformExpression (Set exprs annot)                            = todo
transformExpression (SetComp comprehension annot)                = todo
transformExpression (Starred expr annot)                         = todo
transformExpression (Paren expr annot)                           = (Paren (transformExpression expr) annot)

transformArgument :: Argument t -> Argument t
transformArgument (ArgExpr expr annot) = (ArgExpr (transformExpression expr) annot)

transformSlice :: Slice t -> Slice t
transformSlice (SliceProper lower upper stride annot) = todo
transformSlice (SliceExpr expr annot)                 = todo
transformSlice (SliceEllipsis annot)                  = todo


--Adds n parenthesis around and expression
putParen exp annot = Paren exp annot
putNParen exp annot 0 = putParen exp annot
putNParen exp annot n = putNParen (putParen exp annot) annot (n-1)

-- key, value
-- exempel
constants = [0,"+-1-+1+-1-+1+-1++++1+1+True*2+1-1+1"]



-- Gets the SrcSpan list from python code
f a = unModule $ fst $ (\(Right x) -> x) (parseModule a "")
fb a = fst $ (\(Right x) -> x) (parseModule a "")

 -- some variables to test with pretty and transformExpression --
var = (Var (Ident "x" "") "")
i = (Int 5 "5" "")
binaryOp = (BinaryOp (Plus "") var i "")
unaryOp = Paren (UnaryOp (Minus "") (Int 2 "2" "") "") ""
complexOp = (BinaryOp (Multiply "") var (BinaryOp (Plus "") i binaryOp "") "") 

stmtExtraction x = head $ unModule $ fb x 

a = "x = 5\ny = 8"
b = "x + 10"

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
