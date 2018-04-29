import System.IO
import System.Exit
import System.Environment

import Control.Monad.State

import ParGrammar
import PrintGrammar
import ErrM
import AbsGrammar

-- Type definitions

data SVar = VInt Integer | VBool Bool | VString String | VVoid | VArr [SVar] |
        VRec [(String, SVar)] | VVar String SVar deriving Show

data BreakContinue = BCNone | BNearest | CNearest | BLabel String | CLabel String
data Return = RNone | RReturn SVar

type FEnv = [(String, Fun)]
data Fun = Fun {
    fValArgs :: [String],
    fRefArgs :: [String],
    fStmt :: Stmt,
    fVEnv :: VEnv
}

type VEnv  = [(String, Loc)]
type Store = [(Loc, Maybe SVar)]
type Loc = Integer


data ExecState = ExecState {
    vEnv :: VEnv,
    fEnv :: FEnv,
    store :: Store,
    output :: String,
    location :: Integer,
    breakContinue :: BreakContinue,
    returnState :: Return
}

data ErrorType = SyntaxError | TypeError | ExecutionError
data Result a = Result a | Error ErrorType String String

instance Monad Result where
    return x = Result x
    m >>= f = case m of
        Result res -> f res
        Error t res err -> Error t res err
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap

type Interp a = (StateT ExecState Result) a

-- Framework for running interpreter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [program] -> do
            content <- readFile program
            execute content
        _ -> usage

execute :: String -> IO ()
execute code = case tryExecute code of
    Result s  -> putStr s
    Error errorType partResult errorMSg -> do
        putStr partResult
        case errorType of
            SyntaxError    -> hPutStrLn stderr $ "Syntax error: "    ++ errorMSg
            TypeError      -> hPutStrLn stderr $ "Type error: "      ++ errorMSg
            ExecutionError -> hPutStrLn stderr $ "Execution error: " ++ errorMSg
        exitWith $ ExitFailure 255

tryExecute :: String -> Result String
tryExecute s = do
    ast <- parse s
    runCheck  $ checkProgram ast
    runInterp $ interpretProgram ast

parse :: String -> Result Program
parse str = case pProgram $ myLexer str of
    Ok ast -> Result ast
    Bad msg -> Error SyntaxError "" msg

usage :: IO ()
usage = putStrLn "usage: ./interpreter program"
    
runInterp :: Interp () -> Result String
runInterp m = fmap (output . snd) $ runStateT m emptyExecState

emptyExecState :: ExecState
emptyExecState = ExecState {
    vEnv = [],
    fEnv = [],
    store = [],
    output = "",
    location = 0,
    breakContinue = BCNone,
    returnState = RNone
}

-- Interpreting top-level structures

interpretProgram :: Program -> Interp ()
interpretProgram (Program topDefs) = forM_ topDefs interpretTopDef

interpretTopDef :: TopDef -> Interp ()
interpretTopDef (FnDef funType (Ident "main") args block) = do
    interpretStmt (BStmt block)
    breakContinue <- gets breakContinue
    case breakContinue of
        BCNone -> return ()
        _    -> executionError "end of function reached during break/continue"

interpretTopDef funDef @ (FnDef _ _ _ _) = addFun funDef

interpretTopDef (RecordDef _ _) = return ()

interpretTopDef (VariantDef _ _) = return ()


addFun :: TopDef -> Interp ()
addFun (FnDef _ (Ident funName) args block) = do
    fEnv <- gets fEnv
    vEnv <- gets vEnv
    case lookup funName fEnv of
        Just _  -> executionError $ "multiple function definitions: " ++ funName
        Nothing -> modify $ \s -> s { fEnv = newFEnv }
            where newFEnv = replaceElemKey funName (Fun {
                    fValArgs = parseArgs valArgs,
                    fRefArgs = parseArgs refArgs,
                    fStmt = BStmt block,
                    fVEnv = vEnv
                }) fEnv
                  parseArgs args = map parseArg args
                  parseArg (ArgValDef _ (Ident argName)) = argName
                  parseArg (ArgRefDef _ (Ident argName)) = argName
                  valArgs = filter isVal args
                  refArgs = filter (not . isVal) args
                  isVal arg = case arg of (ArgValDef _ _) -> True; (ArgRefDef _ _) -> False

execFun :: Fun -> [SVar] -> [Loc] -> Interp SVar
execFun f valArgs refArgs = do
    vEnv <- gets vEnv
    modify $ \s -> s { vEnv = fVEnv f }
    addValArgs (fValArgs f) valArgs
    addRefArgs (fRefArgs f) refArgs
    interpretStmt $ fStmt f
    ret <- gets returnState
    modify $ \s -> s { vEnv = vEnv, returnState = RNone }
    return $ case ret of RNone -> VVoid; RReturn retVal -> retVal

addValArgs :: [String] -> [SVar] -> Interp ()
addValArgs [] [] = return ()
addValArgs (name:names) (var:vars) = do
    newVar name (Just var)
    addValArgs names vars
addValArgs a b = executionError $ (show a) ++ " " ++ (show b)

addRefArgs :: [String] -> [Loc] -> Interp ()
addRefArgs [] [] = return ()
addRefArgs (name:names) (loc:locs) = do
    setVarLoc name loc
    addRefArgs names locs
addRefArgs a b = executionError $ (show a) ++ " " ++ (show b)

-- Interpreting statements

interpretStmt :: Stmt -> Interp ()
interpretStmt (BStmt (Block stmts)) = do
    vEnv <- gets vEnv
    fEnv <- gets fEnv
    executeStmts stmts
    modify $ \s -> s { vEnv = vEnv, fEnv = fEnv }

interpretStmt (FStmt funDef) = addFun funDef

interpretStmt (Cond cond ifStmt) = do
    condBool <- calculateBool cond
    if condBool then interpretStmt ifStmt else return ()

interpretStmt (CondElse cond ifStmt elseStmt) = do
    condBool <- calculateBool cond
    interpretStmt $ if condBool then ifStmt else elseStmt

interpretStmt while @ (While cond stmt) = do
    condBool <- calculateBool cond
    breakContinue <- gets breakContinue
    case breakContinue of
        BCNone -> if condBool then interpretStmt stmt >> interpretStmt while else return ()
        BNearest -> modify $ \s -> s { breakContinue = BCNone }
        CNearest -> (modify $ \s -> s { breakContinue = BCNone }) >> interpretStmt while
        _ -> return ()

interpretStmt while @ (WhileAs cond (Ident label) stmt) = do
    condBool <- calculateBool cond
    breakContinue <- gets breakContinue
    case breakContinue of
        BCNone -> if condBool then interpretStmt stmt >> interpretStmt while else return ()
        BNearest -> break
        CNearest -> continue
        BLabel bLabel -> if label == bLabel then break else return ()
        CLabel cLabel -> if label == cLabel then continue else return ()
    where break = modify $ \s -> s { breakContinue = BCNone }
          continue = (modify $ \s -> s { breakContinue = BCNone }) >> interpretStmt while

interpretStmt  Break = modify $ \s -> s { breakContinue = BNearest }

interpretStmt (BreakL (Ident label)) = modify $ \s -> s { breakContinue = BLabel label }

interpretStmt  Continue = modify $ \s -> s { breakContinue = CNearest }

interpretStmt (ContinueL (Ident label)) = modify $ \s -> s { breakContinue = CLabel label }

interpretStmt (Ret val) = do
    ret <- interpretVal val
    modify $ \s -> s { returnState = RReturn ret }

interpretStmt VRet = modify $ \s -> s { returnState = RReturn VVoid }

interpretStmt (VarDecl _ item) = case item of
    NoInit (Ident name)     -> newVar name Nothing
    Init   (Ident name) val -> do
        res <- interpretVal val
        newVar name (Just res)

interpretStmt (BinMod lval AssOp arg) = do
    val <- interpretVal arg
    modifyLVal lval (\_ -> return val)

interpretStmt (BinMod lval PlusEq rval) = do
    val <- interpretVal rval
    case val of 
        VInt valInt       -> modifyLVal lval $ modifyInt (+valInt)
        VString valString -> modifyLVal lval $ modifyString (++valString) 
        _                 -> executionError "oprator += can be used with int or string"

interpretStmt (BinMod lval op val) = do
    valInt <- calculateInt val
    if op == DivEq && valInt == 0 then executionError "Cannot divide by zero"
    else modifyLVal lval $ modifyInt (funMod valInt) where
        funMod valInt = case op of
            MinusEq -> \n -> n - valInt
            TimesEq -> (valInt*)
            DivEq -> \n -> n `div` valInt

interpretStmt (UnMod lval op) = modifyLVal lval (modifyInt funMod) where
    funMod = case op of
        Inc -> (1+)
        Dec -> ((-1)+)


interpretStmt (ValStmt val) = interpretVal val >> return ()

executeStmts :: [Stmt] -> Interp ()
executeStmts stmts = foldr (\m -> \f -> do
        breakContinue <- gets breakContinue
        returnState   <- gets returnState
        case (breakContinue, returnState) of
            (BCNone, RNone) -> m >> f
            _               -> return ()
        ) (return ()) (map interpretStmt stmts)

-- Interpreting values

interpretVal :: Val -> Interp SVar
interpretVal (ELVal lval) = calculateLVal lval

interpretVal (EVar (Var (Ident label) val)) = do
    var <- interpretVal val
    return $ VVar label var

interpretVal (ELitInt n) = return $ VInt n

interpretVal  ELitTrue = return $ VBool True

interpretVal  ELitFalse = return $ VBool False

interpretVal (EString str) = return $ VString str

interpretVal (EApp (Ident funName) args) = if isBuiltIn funName
    then executeBuiltIn funName args
    else do
        fEnv <- gets fEnv
        case lookup funName fEnv of
            Nothing -> executionError $ "unknown function: " ++ funName
            Just f  -> do
                valArgs <- forM (map (\(ArgVal val) -> val) valArgsRaw) interpretVal
                refArgs <- forM (map (\(ArgRef (Ident varName)) -> varName) refArgsRaw)
                    getVarLoc
                execFun f valArgs refArgs >>= return
            where
                valArgsRaw = filter isValArg args
                refArgsRaw = filter (not . isValArg) args
                isValArg arg = case arg of ArgVal _ -> True; ArgRef _ -> False

interpretVal (EArr arr) = do
    values <- mapM interpretVal arr
    return $ VArr values

interpretVal (ERec entries) = do
    values <- mapM interpretRecEntry entries
    return $ VRec values

interpretVal (Neg arg) = do
    argInt <- calculateInt arg
    return $ VInt (-1 * argInt)

interpretVal (Not arg) = do
    argBool <- calculateBool arg
    return $ VBool (not argBool)

interpretVal (EMul lhs op rhs) = do
    lhsInt <- calculateInt lhs
    rhsInt <- calculateInt rhs
    case op of
        Times -> return $ VInt (lhsInt * rhsInt)
        Mod   -> if rhsInt == 0 then executionError "Cannot divide by zero"
                                else return $ VInt (lhsInt `mod` rhsInt)
        Div   -> if rhsInt == 0 then executionError "Cannot divide by zero"
                                else return $ VInt (lhsInt `div` rhsInt)

interpretVal (EAdd lhs Plus rhs) = do
    lhsVal <- interpretVal lhs
    rhsVal <- interpretVal rhs
    case lhsVal of
        VInt lhsInt -> do
            rhsInt <- getInt lhsVal
            return $ VInt (lhsInt + rhsInt)
        VString lhsString -> do
            rhsString <- getString rhsVal
            return $ VString(lhsString ++ rhsString)
        _ -> executionError "oprator + can be used with int or string"

interpretVal (EAdd lhs Minus rhs) = do
    lhsInt <- calculateInt lhs
    rhsInt <- calculateInt rhs
    return $ VInt (lhsInt - rhsInt)

interpretVal (ERel lhs op rhs) = do
    lhsInt <- calculateInt lhs
    rhsInt <- calculateInt rhs
    case op of
        LTH -> return $ VBool(lhsInt < rhsInt)
        LE  -> return $ VBool(lhsInt <= rhsInt)
        GTH -> return $ VBool(lhsInt > rhsInt)
        GE  -> return $ VBool(lhsInt >= rhsInt)
        EQU -> return $ VBool(lhsInt == rhsInt)
        NE  -> return $ VBool(lhsInt /= rhsInt)

interpretVal (EBoolOp lhs op rhs) = do
    lhsBool <- calculateBool lhs
    rhsBool <- calculateBool rhs
    case op of
        And  -> return $ VBool (lhsBool && rhsBool)
        Or   -> return $ VBool (lhsBool || rhsBool)

interpretVal (ECase val cases) = do
    var <- interpretVal val
    case var of
        VVar label varValue -> case findCase label cases of
            Nothing -> executionError "non-exhaustive pattern in case"
            Just (CaseEntry (VarEntry (Ident caseLabel) (Ident varName)) resVal) -> do
                env <- gets vEnv
                newVar varName (Just varValue)
                res <- interpretVal resVal
                modify $ \s -> s { vEnv = env }
                return res
        _ -> executionError "case argument must be variant"

interpretRecEntry :: RecEntry -> Interp (String, SVar)
interpretRecEntry (RecEntry (Ident label) val) = do
    var <- interpretVal val
    return (label, var)

findCase :: String -> [CaseEntry] -> Maybe CaseEntry
findCase _ [] = Nothing
findCase label (c @ (CaseEntry (VarEntry (Ident caseLabel) (Ident var)) _):cases) =
    if label == caseLabel then return c
    else findCase label cases

-- LValue handling

modifyLVal :: LVal -> (SVar -> Interp SVar) -> Interp ()
modifyLVal (LVar (Ident name)) f = do
    var    <- getVar name
    newVar <- f var
    setVar name (Just newVar)

modifyLVal (LArr arr index) f = do
    (VArr arrayVal) <- calculateLVal arr
    indexInt <- calculateInt index
    if indexInt >= toInteger (length arrayVal)
    then executionError $ "Array index out of bounds: " ++ show indexInt
    else do
        newElem <- f (arrayVal !! fromInteger indexInt)
        modifyLVal arr (\_ -> return $ VArr (replaceElemIndex indexInt newElem arrayVal))

modifyLVal (LRec record (Ident field)) f = do
    (VRec recVal) <- calculateLVal record
    case lookup field recVal of
        Nothing  -> executionError $ "record does not have field " ++ field
        Just var -> do
            newVar <- f var
            modifyLVal record (\_ -> return $ VRec (replaceElemKey field newVar recVal))

calculateLVal :: LVal -> Interp SVar

calculateLVal (LVar (Ident name)) = getVar name

calculateLVal (LArr arr index) = do
    (VArr arrayVal) <- calculateLVal arr
    indexInt <- calculateInt index
    if indexInt >= toInteger (length arrayVal)
    then executionError $ "Array index out of bounds: " ++ show indexInt
    else return (arrayVal !! fromInteger indexInt)

calculateLVal (LRec record (Ident field)) = do
    (VRec recVal) <- calculateLVal record
    case lookup field recVal of
        Nothing  -> executionError $ "record does not have field " ++ field
        Just var -> return var


-- Built-in functions handling

isBuiltIn :: String -> Bool
isBuiltIn funName = elem funName ["print", "int_to_string", "bool_to_string"] 

executeBuiltIn :: String -> [Arg] -> Interp SVar
executeBuiltIn "print" args = case args of
    (ArgVal arg:[]) -> do
        str <- interpretVal arg >>= getString
        printToOutput str
        return VVoid
    _ -> executionError "print() call should have exactly one argument\
                \ passed by value"

executeBuiltIn "int_to_string" args = case args of
    (ArgVal arg:[]) -> do
        n <- interpretVal arg >>= getInt
        return $ VString (show n)
    _ -> executionError "int_to_string() call should have exactly one argument\
                \ passed by value"

executeBuiltIn "bool_to_string" args = case args of
    (ArgVal arg:[]) -> do
        bool <- interpretVal arg >>= getBool
        if bool then return $ VString "true" else return $ VString "false"
    _ -> executionError "bool_to_string() call should have exactly one argument\
                \ passed by value"


-- Helper interpreter functions
calculateInt :: Val -> Interp Integer
calculateInt val = interpretVal val >>= getInt

calculateBool :: Val -> Interp Bool
calculateBool val = interpretVal val >>= getBool

getInt :: SVar -> Interp Integer
getInt (VInt n) = return n
getInt var = executionError $ "expected int instead of " ++ (show var)

modifyInt :: (Integer -> Integer) -> SVar -> Interp SVar
modifyInt f var = do
    int <- getInt var
    return $ VInt (f int)

modifyString :: (String -> String) -> SVar -> Interp SVar
modifyString f var = do
    string <- getString var
    return $ VString (f string)

getBool :: SVar -> Interp Bool
getBool (VBool b) = return b
getBool var = executionError $ "expected bool instead of " ++ (show var)

getString :: SVar -> Interp String
getString (VString s) = return s
getString var = executionError $ "expected string instead of " ++ (show var)

printToOutput :: String -> Interp ()
printToOutput str = modify $ \s -> s {output = (output s) ++ str ++ "\n"}

executionError :: String -> Interp a
executionError msg = StateT { runStateT = \s -> Error ExecutionError (output s) msg }

setVar :: String -> Maybe SVar -> Interp ()
setVar name var = do
    env   <- gets vEnv
    store <- gets store
    case lookup name env of
        Just loc -> modify $ \s -> s { store = replaceElemKey loc var store }
        Nothing  -> newVar name var

newVar :: String -> Maybe SVar -> Interp ()
newVar name var = do
    env    <- gets vEnv
    store  <- gets store
    newloc <- getNewLoc
    modify $ \s -> s { vEnv  = replaceElemKey name newloc env,
                       store = replaceElemKey newloc var store }

getVar :: String -> Interp SVar
getVar name = do
    loc   <- getVarLoc name
    store <- gets store
    let (Just var) = lookup loc store
    case var of
        Nothing  -> executionError $ "variable " ++ name ++ " is not initialized"
        Just var -> return var

getVarLoc :: String -> Interp Loc
getVarLoc name = do
    env <- gets vEnv
    case lookup name env of
        Nothing  -> executionError $ "unknown variable: " ++  name
        Just loc -> return loc

setVarLoc :: String -> Loc -> Interp ()
setVarLoc name loc = modify $ \s -> s { vEnv = replaceElemKey name loc (vEnv s) }

getNewLoc :: Interp Integer
getNewLoc = do
    newloc <- gets location
    modify $ \s -> s { location = newloc + 1 }
    return newloc

replaceElemIndex :: Integer -> a -> [a] -> [a]
replaceElemIndex integer new list = (take n list) ++ (new:(drop (n+1) list)) where
    n = fromInteger integer

replaceElemKey :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replaceElemKey key val list = (key, val):(filter (\x -> fst x /= key) list)


-- Type checking

data TCType = TCInt | TCBool | TCString | TCVoid | TCArr TCType |
        TCRec [(String, TCType)] | TCVar [(String, TCType)] | TCTypeName String
        deriving (Show, Eq)

data TCState = TCState {
    tcVEnv :: [(String, TCType)],
    tcFEnv :: [(String, TCFun)],
    tcTypes :: [(String, TCType)]
}

data TCFun = TCFun TCType [TCArg]
data TCArg = TCArgVal TCType String | TCArgRef TCType String


type TypeCheck a = (StateT TCState Result) a

runCheck :: TypeCheck () -> Result ()
runCheck m = fmap (\_ -> ()) $ runStateT m emptyTCState

emptyTCState :: TCState
emptyTCState = TCState {
    tcVEnv = [],
    tcFEnv = [],
    tcTypes = []
}

checkProgram :: Program -> TypeCheck ()
checkProgram (Program topDefs) = forM_ topDefs checkTopDef

checkTopDef :: TopDef -> TypeCheck ()
checkTopDef funDef @ (FnDef _ _ _ _) = tcAddFunDef funDef

-- Type checking in statements

checkStmt :: Stmt -> TypeCheck ()
checkStmt (BStmt (Block stmts)) = do
    vEnv <- gets tcVEnv
    fEnv <- gets tcFEnv
    forM_ stmts checkStmt
    modify $ \s -> s { tcVEnv = vEnv, tcFEnv = fEnv }

checkStmt (FStmt funDef) = tcAddFunDef funDef

checkStmt (Cond cond ifStmt) = typeError "Not yet implemented"

checkStmt (CondElse cond ifStmt elseStmt) = typeError "Not yet implemented"

checkStmt (While cond stmt) = typeError "Not yet implemented"

checkStmt (WhileAs cond _ stmt) = typeError "Not yet implemented"

checkStmt  Break = return ()

checkStmt (BreakL _) = return ()

checkStmt  Continue = return ()

checkStmt (ContinueL _) = return ()

checkStmt (Ret val) = typeError "Not yet implemented"

checkStmt VRet = typeError "Not yet implemented"

checkStmt (VarDecl _ item) = typeError "Not yet implemented"

checkStmt (BinMod lval AssOp arg) = typeError "Not yet implemented"

checkStmt (BinMod lval PlusEq rval) = typeError "Not yet implemented"

checkStmt (BinMod lval op val) = typeError "Not yet implemented"

checkStmt (UnMod lval op) = typeError "Not yet implemented"

checkStmt (ValStmt val) = checkVal val >> return ()

tcAddFunDef :: TopDef -> TypeCheck ()
tcAddFunDef (FnDef retType (Ident funName) args stmt) = do
    modify $ \s -> s {
        tcFEnv = (funName, TCFun (tcParseType retType) (tcParseArgs args)):(tcFEnv s)
    }
    checkStmt $ BStmt stmt


-- Type cheking in values

checkVal :: Val -> TypeCheck TCType
checkVal (ELVal lval) = typeError "Not yet implmemented"

checkVal (EVar (Var (Ident label) val)) = typeError "Not yet implmemented"

checkVal (ELitInt _) = return TCInt

checkVal  ELitTrue = return TCBool

checkVal  ELitFalse = return TCBool

checkVal (EString _) = return TCString

checkVal (EApp (Ident funName) args) = typeError "Not yet implmemented"

checkVal (EArr arr) = typeError "Not yet implmemented"

checkVal (ERec entries) = typeError "Not yet implmemented"

checkVal (Neg arg) = typeError "Not yet implmemented"

checkVal (Not arg) = typeError "Not yet implmemented"

checkVal (EMul lhs op rhs) = typeError "Not yet implmemented"

checkVal (EAdd lhs Plus rhs) = typeError "Not yet implmemented"

checkVal (EAdd lhs Minus rhs) = typeError "Not yet implmemented"

checkVal (ERel lhs op rhs) = typeError "Not yet implmemented"

checkVal (EBoolOp lhs op rhs) = typeError "Not yet implmemented"

checkVal (ECase val cases) = typeError "Not yet implmemented"

-- Helper type checking functions

typeError :: String -> TypeCheck a
typeError msg = StateT { runStateT = \s -> Error TypeError "" msg }

tcParseType :: Type -> TCType
tcParseType Int = TCInt
tcParseType Bool = TCBool
tcParseType Str = TCString
tcParseType Void = TCVoid
tcParseType (UserType (Ident name)) = TCTypeName name
tcParseType (Array innerType) = TCArr (tcParseType innerType)

tcParseArgs :: [ArgDef] -> [TCArg]
tcParseArgs args = map parseArg args where
    parseArg (ArgValDef argType (Ident argName)) = TCArgVal (tcParseType argType) argName
    parseArg (ArgRefDef argType (Ident argName)) = TCArgRef (tcParseType argType) argName
