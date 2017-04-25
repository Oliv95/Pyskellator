import Language.Python.Common
import Language.Python.Version3.Parser
import qualified Data.Map as Map
import Data.List(intersperse)
import System.Random

stmListFromSource :: String -> String -> ModuleSpan
stmListFromSource scoure fileName = fst $ (\(Right x) -> x) (parseModule scoure fileName)

unModule :: ModuleSpan -> [Statement SrcSpan] 
unModule (Module xs) = xs

todo = error "Not implemented"

transformStatement :: Statement t -> Statement t
transformStatement (While cond body whileElse annot)         = While newCond newBody newElse annot
            where newCond = transformExpression cond
                  newBody = fmap transformStatement body
                  newElse = fmap transformStatement whileElse
transformStatement a@(For vars generator body forElse annot) = For vars newGen newBody newElse annot
            where newVars = fmap transformExpression vars
                  newGen  = transformExpression generator
                  newBody = fmap transformStatement body
                  newElse = fmap transformStatement forElse
transformStatement (Fun name params resAnnot body annot)     = Fun name params resAnnot newBody annot
            where newBody = fmap transformStatement body
transformStatement (Class name args body annot)              = Class name newArgs newBody annot
            where newArgs = fmap transformArgument args
                  newBody = fmap transformStatement body
transformStatement (Conditional condGuards condElse annot)   = Conditional newGuards newElse annot
            where newGuards = fmap transformGuard condGuards
                  newElse   = fmap transformStatement condElse
                  transformGuard = \(e,s) -> (transformExpression e,fmap transformStatement s)
transformStatement (Assign target expr annot)                = (Assign target (transformExpression expr) annot)
transformStatement (AugmentedAssign target op expr annot)    = AugmentedAssign target op newExpr annot
            where newExpr = transformExpression expr
transformStatement (Decorated decorators definition annot)   = Decorated newDecor newDef annot
            where newDecor = fmap transformDecorator decorators
                  newDef   = transformStatement definition
transformStatement a@(Return expr annot)              = Return newExpr annot
            where newExpr = fmap transformExpression expr
transformStatement a@(Try try except tryElse finally annot)  = Try newTry newExcept newElse newFinally annot
            where newTry = fmap transformStatement try
                  newExcept = fmap transformHandler except
                  newElse   = fmap transformStatement tryElse
                  newFinally = fmap transformStatement finally
transformStatement a@(Raise raise annot)                      = Raise newRaise annot
            where newRaise = transformRaise raise
transformStatement a@(With context body annot)               = With newContext newBody annot
            where newContext = fmap transformContext context
                  newBody   = fmap transformStatement body
                  transformContext = \(e,maybeE) -> (transformExpression e,fmap transformExpression maybeE)
transformStatement (StmtExpr expr annot)                     = (StmtExpr (transformExpression expr) annot)
transformStatement a@(Assert exprs annot)                    = Assert newExprs annot
            where newExprs = fmap transformExpression exprs
transformStatement a@(Delete exprs annot)                    = a -- Not sure about this one
transformStatement s = s

transformExpression :: Expr t -> Expr t
transformExpression var@(Var ident annot) = Paren (Call lambda lambdaArgs annot) annot
    where args = [Param ident Nothing Nothing annot]
          body = var
          lambda = Paren (Lambda args body annot) annot
          lambdaArgs = [ArgExpr var annot]
transformExpression e@(Int value lit annot)                        = newValue
    where lambda = Paren (Lambda [] e annot) annot
          newValue = Paren (Call lambda [] annot) annot
transformExpression a@(Float value lit annot)                      = a
transformExpression a@(Imaginary value lit annot)                  = a
transformExpression a@(Bool True annot)                            = a
transformExpression a@(Bool False annot)                           = a
transformExpression a@(None annot)                                 = a
transformExpression a@(Ellipsis annot)                             = a
transformExpression a@(ByteStrings strings annot)                  = a
transformExpression a@(Strings strings annot)                      = a
transformExpression a@(Call expr args annot)                       = a
transformExpression a@(Subscript target index annot)               = a
transformExpression a@(SlicedExpr traget slices annot)             = a
transformExpression a@(CondExpr trueBranch cond falseBranch annot) = a
transformExpression (BinaryOp op left right annot)               = Call lambda [] annot
    where leftExpr  = Paren (transformExpression left) annot 
          rightExpr = Paren (transformExpression right) annot
          operation = Paren (BinaryOp op leftExpr rightExpr annot) annot
          lambda    = Paren (Lambda [] (Paren operation annot) annot) annot
transformExpression (UnaryOp op expr annot)                      = (Paren (UnaryOp op (transformExpression expr) annot) annot)
transformExpression a@(Dot expr attribute annot)                   = a
transformExpression a@(Lambda params body annot)                   = a
transformExpression a@(Tuple exprs annot)                          = a
transformExpression a@(Yield arg annot)                            = a
transformExpression a@(Generator comprehension  annot)             = a
transformExpression a@(ListComp  comprehension annot)              = a
transformExpression a@(List list annot)                            = a
transformExpression a@(Dictionary mappings annot)                  = a
transformExpression a@(DictComp comprehension annot)               = a
transformExpression a@(Set exprs annot)                            = a
transformExpression a@(SetComp comprehension annot)                = a
transformExpression a@(Starred expr annot)                         = a
transformExpression (Paren expr annot)                           = (Paren (transformExpression expr) annot)

transformArgument :: Argument t -> Argument t
transformArgument (ArgExpr expr annot) = (ArgExpr (transformExpression expr) annot)
transformArgument a                    = a

transformSlice :: Slice t -> Slice t
transformSlice (SliceProper lower upper stride annot) = todo
transformSlice (SliceExpr expr annot)                 = todo
transformSlice (SliceEllipsis annot)                  = todo

transformDecorator :: Decorator a -> Decorator a
transformDecorator (Decorator name args annot) = Decorator name newArgs annot
        where newArgs = fmap transformArgument args

transformHandler :: Handler a -> Handler a
transformHandler (Handler except stms annot) = Handler except newStms annot
        where newStms = fmap transformStatement stms

transformRaise :: RaiseExpr a -> RaiseExpr a
transformRaise = id

--Adds n parenthesis around and expression
putParen exp annot = Paren exp annot
putNParen exp annot 0 = putParen exp annot
putNParen exp annot n = putNParen (putParen exp annot) annot (n-1)

-- key, value
-- exempel
constants = (0,"+-1-+1+-1-+1+-1++++1+1+True*2+1-1+1")


data Constant = Empty                  |
                CNumber Int            |
                CBoolean Bool          |
                CList   [Constant]     |
                CTuple  [Constant]     |
                Parenthesis Constant   |
                UOperator UOP Constant | 
                BOperator BOP Constant Constant --BOP Constants need to be inside parenthesis

-- Identity is +x, negate -x , comp is ~x
data UOP = Identity | Negate | Comp

-- Add is x+y, sub is x-y, Mul is x*y, LShift is x << y
data BOP = Add | Sub | Mul | LShift

class Pythonable a where
    pythonize :: a -> String

instance Pythonable BOP where
    pythonize Add = "+"
    pythonize Sub = "-"
    pythonize Mul = "*"
    pythonize LShift = "<<"

instance Pythonable UOP where
    pythonize Identity = "+"
    pythonize Negate   = "-"
    pythonize Comp     = "~"

instance Pythonable Constant where
    pythonize Empty                = ""
    pythonize (CNumber n)          = show n
    pythonize (CBoolean b)         = show b
    pythonize (CList l)            = "bool([" ++ content ++ "])"
            where content = concat $ intersperse "," (fmap pythonize l)
    pythonize (CTuple t)           = "bool((" ++ content ++ "))"
            where content = concat $ intersperse "," (fmap pythonize t)
    pythonize (Parenthesis c)      = "(" ++ (pythonize c) ++ ")"
    pythonize (UOperator op c)     = pythonize op ++ pythonize c
    pythonize (BOperator op c1 c2) = pythonize c1 ++ pythonize op ++ pythonize c2

randomNum :: IO Int
randomNum = getStdRandom (randomR (0,1))                


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

testFile path outfile = do
    content <- readFile path
    let mod = unModule $ stmListFromSource content ""
    let res = fmap transformStatement mod
    let obfu =  unlines (fmap show (fmap pretty res))
    putStrLn obfu
    writeFile outfile obfu
    return ()
    
    
    

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
