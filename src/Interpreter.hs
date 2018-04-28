import System.IO
import System.Exit
import System.Environment

import Control.Monad.State

import ParGrammar
import PrintGrammar
import ErrM
import AbsGrammar

-- Type definitions

data SVar = VInt Integer | VBool Bool | VString String | VVoid | VArr [SVar]
    deriving Show
type Loc = Integer
type Fun = ([SVar] -> SVar)

type VEnv  = [(String, Loc)]
type Store = [(Loc, Maybe SVar)]


data ExecState = ExecState {
    vEnv :: VEnv,
    store :: Store,
    output :: String,
    location :: Integer
}

data Result a = Result a | Error String String

instance Monad Result where
    return x = Result x
    m >>= f = case m of
        Result res -> f res
        Error res err -> Error res err
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
    Error partResult errorMSg -> do
        putStr partResult
        hPutStrLn stderr $ "Execution error: " ++ errorMSg
        exitWith $ ExitFailure 1

tryExecute :: String -> Result String
tryExecute s = do
    ast <- parse s
    runInterp $ interpretProgram ast

parse :: String -> Result Program
parse str = case pProgram $ myLexer str of
    Ok ast -> Result ast
    Bad msg -> Error "" msg

usage :: IO ()
usage = putStrLn "usage: ./interpreter program"
    
runInterp :: Interp () -> Result String
runInterp m = fmap (output . snd) $ runStateT m emptyExecState

emptyExecState :: ExecState
emptyExecState = ExecState {vEnv = [], store = [], output = "", location = 0}

-- Interpreting top-level structures

interpretProgram :: Program -> Interp ()
interpretProgram (Program topDefs) = forM_ topDefs interpretTopDef

interpretTopDef :: TopDef -> Interp ()
interpretTopDef (TypeFnDef funType (Ident "main") args (Block cmds)) = forM_ cmds interpretStmt
interpretTopDef (TypeFnDef funType (Ident funName) args (Block cmds)) = return () --TODO


-- Interpreting statements

interpretStmt :: Stmt -> Interp ()
interpretStmt (BStmt (Block cmds)) = forM_ cmds interpretStmt

interpretStmt (FStmt funDef) = reportError "Not yet implemented FStmt"

interpretStmt (Cond cond ifStmt) = reportError "Not yet implemented Cond"

interpretStmt (CondElse cond ifStmt elseStmt) = reportError "Not yet implemented CondElse"

interpretStmt (While cond stmt) = reportError "Not yet implemented While"

interpretStmt (WhileAs cond label stmt) = reportError "Not yet implemented WhileAs"

interpretStmt  Break = reportError "Not yet implemented Break"

interpretStmt (BreakL label) = reportError "Not yet implemented BreakL"

interpretStmt  Continue = reportError "Not yet implemented Continue"

interpretStmt (ContinueL label) = reportError "Not yet implemented ContinueL"

interpretStmt (Ret val) = reportError "Not yet implemented Ret"

interpretStmt VRet = reportError "Not yet implemented VRet"

interpretStmt (VarDecl _ item) = case item of
    NoInit (Ident name)     -> setVar name Nothing
    Init   (Ident name) val -> do
        res <- interpretVal val
        setVar name (Just res)

interpretStmt (BinMod lval AssOp arg) = do
    val <- interpretVal arg
    modifyLVal lval (\_ -> return val)

interpretStmt (BinMod lval op val) = do
    valInt <- calculateInt val
    if op == DivEq && valInt == 0 then reportError "Cannot divide by zero"
    else modifyLVal lval $ modifyInt (funMod valInt) where
        funMod valInt = case op of
            PlusEq -> (valInt+)
            MinusEq -> \n -> n - valInt
            TimesEq -> (valInt*)
            DivEq -> \n -> n `div` valInt

interpretStmt (UnMod lval op) = modifyLVal lval (modifyInt funMod) where
    funMod = case op of
        Inc -> (1+)
        Dec -> ((-1)+)


interpretStmt (ValStmt val) = interpretVal val >> return ()

-- Interpreting values

interpretVal :: Val -> Interp SVar
interpretVal (ELVal lval) = calculateLVal lval

interpretVal (EVar (Var label val)) = reportError "Not yet implemented EVar"

interpretVal (ELitInt n) = return $ VInt n

interpretVal  ELitTrue = return $ VBool True

interpretVal  ELitFalse = return $ VBool False

interpretVal (EString str) = return $ VString str

interpretVal (EApp (Ident funName) args) = if isBuiltIn funName
    then executeBuiltIn funName args
    else reportError "Not yet implemented EApp"

interpretVal (EArr arr) = do
    values <- mapM interpretVal arr
    return $ VArr values

interpretVal (ERec rec) = reportError "Not yet implemented ERec"

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
        Mod   -> if rhsInt == 0 then reportError "Cannot divide by zero"
                                else return $ VInt (lhsInt `mod` rhsInt)
        Div   -> if rhsInt == 0 then reportError "Cannot divide by zero"
                                else return $ VInt (lhsInt `div` rhsInt)

interpretVal (EAdd lhs op rhs) = do
    lhsInt <- calculateInt lhs
    rhsInt <- calculateInt rhs
    case op of
        Plus  -> return $ VInt (lhsInt + rhsInt)
        Minus -> return $ VInt (lhsInt - rhsInt)

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

interpretVal (ECase val cases) = reportError "Not yet implemented ECase"

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
    then reportError $ "Array index out of bounds: " ++ show indexInt
    else do
        newElem <- f (arrayVal !! fromInteger indexInt)
        modifyLVal arr (\_ -> return $ VArr (replaceElem indexInt newElem arrayVal))

modifyLVal (LRec record (Ident field)) f = reportError "Not yet implemented LRec"


calculateLVal :: LVal -> Interp SVar

calculateLVal (LVar (Ident name)) = getVar name

calculateLVal (LArr arr index) = do
    (VArr arrayVal) <- calculateLVal arr
    indexInt <- calculateInt index
    if indexInt >= toInteger (length arrayVal)
    then reportError $ "Array index out of bounds: " ++ show indexInt
    else return (arrayVal !! fromInteger indexInt)

calculateLVal (LRec record (Ident field)) = reportError "Not yet implemented LRec"


-- Built-in functions handling

isBuiltIn :: String -> Bool
isBuiltIn funName = elem funName ["print", "int_to_string", "bool_to_string"] 

executeBuiltIn :: String -> [Arg] -> Interp SVar
executeBuiltIn "print" args = case args of
    (ArgVal arg:[]) -> do
        str <- interpretVal arg >>= getString
        printToOutput str
        return VVoid
    _ -> reportError "print() call should have exactly one argument\
                \ passed by value"

executeBuiltIn "int_to_string" args = case args of
    (ArgVal arg:[]) -> do
        n <- interpretVal arg >>= getInt
        return $ VString (show n)
    _ -> reportError "int_to_string() call should have exactly one argument\
                \ passed by value"

executeBuiltIn "bool_to_string" args = case args of
    (ArgVal arg:[]) -> do
        bool <- interpretVal arg >>= getBool
        if bool then return $ VString "true" else return $ VString "false"
    _ -> reportError "bool_to_string() call should have exactly one argument\
                \ passed by value"


-- Helper functions
calculateInt :: Val -> Interp Integer
calculateInt val = interpretVal val >>= getInt

calculateBool :: Val -> Interp Bool
calculateBool val = interpretVal val >>= getBool

getInt :: SVar -> Interp Integer
getInt (VInt n) = return n
getInt var = reportError $ "expected int instead of " ++ (show var)

modifyInt :: (Integer -> Integer) -> SVar -> Interp SVar
modifyInt f var = do
    int <- getInt var
    return $ VInt (f int)

getBool :: SVar -> Interp Bool
getBool (VBool b) = return b
getBool var = reportError $ "expected bool instead of " ++ (show var)

getString :: SVar -> Interp String
getString (VString s) = return s
getString var = reportError $ "expected string instead of " ++ (show var)

printToOutput :: String -> Interp ()
printToOutput str = modify $ \s -> s {output = (output s) ++ str ++ "\n"}

reportError :: String -> Interp a
reportError msg = StateT { runStateT = \s -> Error (output s) msg }

setVar :: String -> Maybe SVar -> Interp ()
setVar name var = do
    env   <- gets vEnv
    store <- gets store
    case lookup name env of
        Just loc -> modify $ \s -> s { store = setElem loc var store }
        Nothing  -> do
                    newloc <- getNewLoc
                    modify $ \s -> s { vEnv  = setElem name newloc env,
                                       store = setElem newloc var store }
        where
        setElem key val list = (key, val):(filter (\x -> fst x /= key) list)

getVar :: String -> Interp SVar
getVar name = do
    loc   <- getVarLoc name
    store <- gets store
    let (Just var) = lookup loc store
    case var of
        Nothing  -> reportError $ "variable " ++ name ++ " is not initialized"
        Just var -> return var

getVarLoc :: String -> Interp Loc
getVarLoc name = do
    env <- gets vEnv
    case lookup name env of
        Nothing  -> reportError $ "unknown variable: " ++  name
        Just loc -> return loc

getNewLoc :: Interp Integer
getNewLoc = do
    newloc <- gets location
    modify $ \s -> s { location = newloc + 1 }
    return newloc

replaceElem :: Integer -> a -> [a] -> [a]
replaceElem integer new list = (take n list) ++ (new:(drop (n+1) list)) where
    n = fromInteger integer
