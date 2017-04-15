import Language.Python.Common
import Language.Python.Version3.Parser

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
transformStatement (StmtExpr expr annot)                   = todo
transformStatement (Global vars annot)                     = todo
transformStatement (NonLocal vars annot)                   = todo
transformStatement (Assert exprs annot)                    = todo

transformExpression :: Expr t -> Expr t
transformExpression (Var indent annot)                           = todo
transformExpression (Int value lit annot)                        = todo
transformExpression (Float value lit annot)                      = todo
transformExpression (Imaginary value lit annot)                  = todo
transformExpression (Bool value annot)                           = todo
transformExpression (None annot)                                 = todo
transformExpression (Ellipsis annot)                             = todo
transformExpression (ByteStrings strings annot)                  = todo
transformExpression (Strings strings annot)                      = todo
transformExpression (Call expr args annot)                       = todo
transformExpression (Subscript target index annot)               = todo
transformExpression (SlicedExpr traget slices annot)             = todo
transformExpression (CondExpr trueBranch cond falseBranch annot) = todo
transformExpression (BinaryOp op left right annot)               = todo
transformExpression (UnaryOp op arg annot)                       = todo
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
transformExpression (Paren expr annot)                           = todo

transformSlice :: Slice t -> Slice t
transformSlice (SliceProper lower upper stride annot) = todo
transformSlice (SliceExpr expr annot)                 = todo
transformSlice (SliceEllipsis annot)                  = todo



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
