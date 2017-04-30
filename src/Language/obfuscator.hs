import System.Process
import System.IO
import Language.Python.Common
import Language.Python.Version3.Parser
import qualified Data.Map as Map
import Data.List(intersperse)
import System.Random
import Test.QuickCheck

stmListFromSource :: String -> String -> ModuleSpan
stmListFromSource scoure fileName = fst $ (\(Right x) -> x) (parseModule scoure fileName)

unModule :: ModuleSpan -> [Statement SrcSpan] 
unModule (Module xs) = xs

todo = error "Not implemented"

transformSuite :: [Statement SrcSpan] -> IO [Statement SrcSpan]
transformSuite = mapM transformStatement

transformStatement :: Statement SrcSpan -> IO (Statement SrcSpan)
transformStatement (While cond body whileElse annot)         = do
                  newCond <- transformExpression cond
                  newBody <- transformSuite body
                  newElse <- transformSuite whileElse
                  return $ While newCond newBody newElse annot
transformStatement a@(For vars generator body forElse annot) = do
                  newVars <- mapM transformExpression vars
                  newGen  <- transformExpression generator
                  newBody <- transformSuite body
                  newElse <- transformSuite forElse
                  return $ For vars newGen newBody newElse annot
transformStatement (Fun name params resAnnot body annot)     = do
            newBody <- transformSuite body
            return $ Fun name params resAnnot newBody annot
transformStatement (Class name args body annot)              = do
                  newArgs <- mapM transformArgument args
                  newBody <- transformSuite body
                  return $ Class name newArgs newBody annot
transformStatement (Conditional condGuards condElse annot)   = do
                  let transformGuard (e,s) = do
                                expr <- transformExpression e
                                suit <- transformSuite s
                                return (expr,suit)
                  newGuards <- mapM transformGuard condGuards
                  newElse   <- transformSuite condElse
                  return $ Conditional newGuards newElse annot
transformStatement (Assign target expr annot)                = do
            obExpr <- transformExpression expr
            return $ (Assign target obExpr annot)
transformStatement (AugmentedAssign target op expr annot)    = do
            newExpr <- transformExpression expr
            return $ AugmentedAssign target op newExpr annot
transformStatement (Decorated decorators definition annot)   = do
                  newDecor <- mapM transformDecorator decorators
                  newDef   <- transformStatement definition
                  return $ Decorated newDecor newDef annot
transformStatement a@(Return expr annot)              = do
            newExpr <- mapM transformExpression expr
            return $ Return newExpr annot
transformStatement a@(Try try except tryElse finally annot)  = do 
                  newTry <- transformSuite try
                  newExcept <- mapM transformHandler except
                  newElse   <- transformSuite tryElse
                  newFinally <- transformSuite finally
                  return $ Try newTry newExcept newElse newFinally annot
transformStatement a@(Raise raise annot)                      = do 
            newRaise <- transformRaise raise
            return $ Raise newRaise annot
transformStatement a@(With context body annot)               = do
                  let transformContext (e,maybeE) = do
                                    expr <- transformExpression e
                                    mndgr <- mapM transformExpression maybeE
                                    return (expr,mndgr)
                  newContext <- mapM transformContext context
                  newBody   <- transformSuite body
                  return $ With newContext newBody annot
transformStatement (StmtExpr expr annot)                     = do 
            obExpr <- transformExpression expr
            return $ (StmtExpr (obExpr) annot)
transformStatement a@(Assert exprs annot)                    = do
            newExprs <- mapM transformExpression exprs
            return $ Assert newExprs annot
transformStatement a@(Delete exprs annot)                    = return $ a -- Not sure about this one
transformStatement s = return $ s

transformExpression :: Expr SrcSpan -> IO (Expr SrcSpan)
transformExpression var@(Var ident annot) = do
          let args = [Param ident Nothing Nothing annot]
          let body = var
          let lambda = Paren (Lambda args body annot) annot
          let lambdaArgs = [ArgExpr var annot]
          return $ Paren (Call lambda lambdaArgs annot) annot
transformExpression e@(Int value lit annot)                        = do 
          obfuConstant <- getObfuscated (fromIntegral value)
          let expr  = constantTOExpr obfuConstant
          let lambda = Paren (Lambda [] expr annot) annot
          let newValue = Paren (Call lambda [] annot) annot
          return newValue
transformExpression a@(Float value lit annot)                      = return a
transformExpression a@(Imaginary value lit annot)                  = return a
transformExpression a@(Bool b annot)                            = do
                                        constant <- if b then getObfuscated 1 else getObfuscated 0
                                        let toBool = CList [constant]
                                        let expr   = constantTOExpr constant
                                        let lambda = Paren (Lambda [] expr annot) annot
                                        let newValue = Paren (Call lambda [] annot) annot
                                        return newValue

transformExpression a@(None annot)                                 = return a -- Leave these two?
transformExpression a@(Ellipsis annot)                             = return a
transformExpression a@(ByteStrings strings annot)                  = do
                                    return (ByteStrings (fmap transformString strings) annot)
transformExpression a@(Strings strings annot)                      = do 
                                    return (Strings (fmap transformString strings) annot)
transformExpression a@(Call expr args annot)                       = do
                    obfuExpr <- transformExpression expr
                    obfuArgs <- mapM transformArgument args
                    return (Call obfuExpr obfuArgs annot)
transformExpression a@(Subscript target index annot)               = do
                    obfuTarget <- transformExpression target
                    obfuIndex <- transformExpression index
                    return (Subscript obfuTarget obfuIndex annot)
transformExpression a@(SlicedExpr target slices annot)             = do
                    obfuTarget <- transformExpression target
                    obfuSlice  <- mapM transformSlice slices
                    return (SlicedExpr obfuTarget obfuSlice annot)
transformExpression a@(CondExpr trueBranch cond falseBranch annot) = do
                    obfuTrue <- transformExpression trueBranch
                    obfuCond <- transformExpression cond
                    obfuFalse <- transformExpression falseBranch
                    return (CondExpr obfuTrue obfuCond obfuFalse annot)
transformExpression (BinaryOp op left right annot)               = do 
          leftExpr  <- transformExpression left
          rightExpr <- transformExpression right
          let operation = Paren (BinaryOp op (Paren leftExpr annot) (Paren rightExpr annot) annot) annot
          let lambda = Paren (Lambda [] (Paren operation annot) annot) annot
          return $ Call lambda [] annot
transformExpression (UnaryOp op expr annot)                        = do 
                   obExpr <- transformExpression expr
                   return $ (Paren (UnaryOp op obExpr annot) annot)
transformExpression a@(Dot expr attribute annot)                   = do
                   obfuExpr <- transformExpression expr
                   return (Dot obfuExpr attribute annot)
transformExpression a@(Lambda params body annot)                   = do
                   obfuPara <- mapM transformParam params
                   obfuBody <- transformExpression body
                   return (Lambda obfuPara obfuBody annot)
transformExpression a@(Tuple exprs annot)                          = do
                   obfuExpr <- mapM transformExpression exprs
                   return (Tuple obfuExpr annot)
transformExpression a@(Yield arg annot)                            = do
                   obfuArg <- mapM transformYield arg
                   return (Yield obfuArg annot)
transformExpression a@(Generator comprehension  annot)             = do
                   obfuComp <- transformComprehension comprehension
                   return (Generator obfuComp annot)
transformExpression a@(ListComp  comprehension annot)              = do
                   obfuComp <- transformComprehension comprehension
                   return (Generator obfuComp annot)
transformExpression a@(List list annot)                            = do
                   obfuList <- mapM transformExpression list
                   return (List obfuList annot)
transformExpression a@(Dictionary mappings annot)                  = do
                   obfuMapping <- mapM transformDictMap mappings
                   return (Dictionary obfuMapping annot)
transformExpression a@(DictComp comprehension annot)               = do
                   obfuComp <- transformComprehension comprehension
                   return (DictComp obfuComp annot)
transformExpression a@(Set exprs annot)                            = do
                   obfuExpr <- mapM transformExpression exprs
                   return (Set obfuExpr annot)
transformExpression a@(SetComp comprehension annot)                = do
                   obfuComp <- transformComprehension comprehension
                   return (SetComp obfuComp annot)
transformExpression a@(Starred expr annot)                         = do
                   obfuExpr <- transformExpression expr
                   return (Starred obfuExpr annot)
transformExpression (Paren expr annot)                             = do
                        obExpr <- transformExpression expr
                        return (Paren obExpr annot)

constantTOExpr :: Constant -> Expr SrcSpan
constantTOExpr constant = expr
          where parse = parseExpr (pythonize constant) ""
                expr  = (\(Right (exp,tl)) -> exp) parse

transformDictMap :: DictMappingPair SrcSpan -> IO (DictMappingPair SrcSpan)
transformDictMap (DictMappingPair expr1 expr2) = do
                obfuExpr1 <- transformExpression expr1
                obfuExpr2 <- transformExpression expr2
                return (DictMappingPair obfuExpr1 obfuExpr2)

transformYield :: YieldArg SrcSpan -> IO (YieldArg SrcSpan)
transformYield (YieldFrom expr annot) = do
            obfuExpr <- transformExpression expr
            return (YieldFrom obfuExpr annot)
transformYield (YieldExpr expr) = do
            obfuExpr <- transformExpression expr
            return (YieldExpr obfuExpr)

transformParam :: Parameter SrcSpan -> IO (Parameter SrcSpan)
transformParam = return --Maybe leave this one

transformString :: String -> String
transformString = todo

transformArgument :: Argument SrcSpan -> IO (Argument SrcSpan)
transformArgument (ArgExpr expr annot) = do
                    exp <- transformExpression expr
                    return (ArgExpr (exp) annot)
transformArgument a                    = return a

transformSlice :: Slice SrcSpan -> IO (Slice SrcSpan)
transformSlice (SliceProper lower upper stride annot) = todo
transformSlice (SliceExpr expr annot)                 = todo
transformSlice (SliceEllipsis annot)                  = todo

transformDecorator :: Decorator SrcSpan -> IO (Decorator SrcSpan)
transformDecorator (Decorator name args annot) = do
        newArgs <- mapM transformArgument args
        return $ Decorator name newArgs annot

transformHandler :: Handler SrcSpan -> IO (Handler SrcSpan)
transformHandler (Handler except stms annot) = do 
                    newStms <- transformSuite stms
                    return $ Handler except newStms annot

transformRaise :: RaiseExpr SrcSpan -> IO (RaiseExpr SrcSpan)
transformRaise = todo

transformComprehension :: Comprehension SrcSpan-> IO (Comprehension SrcSpan)
transformComprehension (Comprehension compExpr compFor annot) = do
                obfuCompExpr <- transformComprehensionExpr compExpr
                obfuCompFor <- transformCompFor compFor 
                -- This one is fucked, might be pretty that fucks it up
                return (Comprehension compExpr compFor annot)

transformCompFor (CompFor exprs expr mIter annot) = do
                obfuExprs <- mapM transformExpression exprs
                obfuExpr  <- transformExpression expr
                return (CompFor obfuExprs obfuExpr mIter annot)

transformComprehensionExpr (ComprehensionExpr expr)   = do
                        exp <- transformExpression expr
                        return (ComprehensionExpr exp)
transformComprehensionExpr (ComprehensionDict  mapping) = do 
                        obfuMapping <- transformDictMap mapping
                        return (ComprehensionDict obfuMapping)

--Adds n parenthesis around and expression
putParen exp annot = Paren exp annot
putNParen exp annot 0 = putParen exp annot
putNParen exp annot n = putNParen (putParen exp annot) annot (n-1)

-- negative exponent, negative shift
data Constant = CNumber Int            |
                CBoolean Bool          |
                CList   [Constant]     |
                Parenthesis Constant   |
                UOperator UOP Constant | 
                BOperator BOP Constant Constant --BOP Constants need to be inside parenthesis
    deriving (Show,Eq)

-- Identity is +x, negate -x , comp is ~x
data UOP = Identity | Negate | Comp
    deriving (Show,Eq)

-- Add is x+y, sub is x-y, Mul is x*y, LShift is x << y
data BOP = Add | Sub | Mul | LShift
    deriving (Show,Eq)

instance Arbitrary UOP where
    arbitrary = elements [Identity, Negate, Comp]
instance Arbitrary BOP where
    arbitrary = elements [Add, Sub, Mul, LShift]
instance Arbitrary Constant where
    arbitrary = do
            n <- suchThat arbitrary (\x -> x >= 0)
            b <- arbitrary
            l <- arbitrary
            uop <- arbitrary
            bop <- arbitrary
            c   <- arbitrary
            let possibilities = [CNumber n,CBoolean b,CList l,UOperator uop c,BOperator bop c (CNumber n),Parenthesis c]
            let gens = map (\x -> elements [x]) possibilities
            let weights = [1,1,1,15,15,10]
            frequency $ zip weights gens


getObfuscated :: Int -> IO Constant
getObfuscated target  = do
        let g = arbitrary :: Gen Constant
        let gen = resize 1 g
        constant <- generate gen
        let value = evalConstant constant
        let diff = target - value
        return (BOperator Add (CNumber diff) constant )

test2 target = do
    c <- getObfuscated target
    let v = evalConstant c
    putStrLn (show target)
    putStrLn (pythonize c)
    putStrLn (show v)
    evalInPython (pythonize c)

test = do
    let g = arbitrary :: Gen Constant
    let k = resize 1 g
    value <- generate k
    let a = evalConstant value
    putStrLn (show value)
    putStrLn (pythonize value)
    putStrLn ""
    putStrLn (show a)
    evalInPython (pythonize value)

evalInPython expr = do
            callCommand (cmd expr)
cmd expr = "python3 -c \"print( "++ expr ++")\""
        
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
    pythonize (CNumber n)          = show n
    pythonize (CBoolean b)         = show b
    pythonize (CList l)            = "bool([" ++ content ++ "])"
            where content = concat $ intersperse "," (fmap pythonize l)
    pythonize (Parenthesis c)      = "(" ++ (pythonize c) ++ ")"
    pythonize (UOperator op c)     = pythonize op ++ pythonize c
    pythonize (BOperator op c1 c2) = "(" ++ pythonize c1 ++ pythonize op ++ pythonize c2 ++ ")"

evalConstant :: Constant -> Int
evalConstant (CNumber n)      = n
evalConstant (CBoolean b)     = if b then 1 else 0
evalConstant (CList l)        = if null l then 0 else 1
evalConstant (Parenthesis c)  = evalConstant c
evalConstant (UOperator op c) = case op of 
                        Identity -> evalConstant c
                        Negate   -> -(evalConstant c)
                        Comp     -> (-(evalConstant c)) - 1
evalConstant (BOperator op c c1)      = case op of
                        Add    -> evalConstant c + evalConstant c1
                        Sub    -> evalConstant c - evalConstant c1
                        Mul    -> evalConstant c * evalConstant c1
                        LShift -> (evalConstant c) * (2^(evalConstant c1))



 -- some variables to test with pretty and transformExpression --
var = (Var (Ident "x" "") "")
i = (Int 5 "5" "")
binaryOp = (BinaryOp (Plus "") var i "")
unaryOp = Paren (UnaryOp (Minus "") (Int 2 "2" "") "") ""
complexOp = (BinaryOp (Multiply "") var (BinaryOp (Plus "") i binaryOp "") "") 

a = "x = 5\ny = 8"
b = "x + 10"

testFile path outfile = do
    content <- readFile path
    let mod = unModule $ stmListFromSource content ""
    res <- mapM transformStatement mod
    let obfu =  unlines (fmap show (fmap pretty res))
    putStrLn obfu
    writeFile outfile obfu
  
