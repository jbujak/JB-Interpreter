import System.IO
import System.Exit
import System.Environment

import Control.Monad.State

import ParGrammar
import PrintGrammar
import ErrM
import AbsGrammar

data SVar = VInt Integer | VBool Bool | VString String | VVoid deriving Show
type Loc = Integer
type Fun = ([SVar] -> SVar)

type VEnv = [(String, Loc)]
type Store = [(Loc, String)]


data ExecState = ExecState {
    vEnv :: VEnv,
    store :: Store,
    output :: String
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


-- Interpreting top-level structures

interpretProgram :: Program -> Interp ()
interpretProgram (Program topDefs) = forM_ topDefs interpretTopDef

interpretTopDef :: TopDef -> Interp ()
interpretTopDef (TypeFnDef funType (Ident "main") args (Block cmds)) = forM_ cmds interpretStmt
interpretTopDef (TypeFnDef funType (Ident funName) args (Block cmds)) = return () --TODO


-- Interpreting statements

interpretStmt :: Stmt -> Interp ()
interpretStmt (BStmt (Block cmds)) = forM_ cmds interpretStmt
interpretStmt (ValStmt val) = interpretVal val >> return ()


-- Interpreting values

interpretVal :: Val -> Interp SVar
interpretVal (ELval lval) = reportError "Not yet implemented ELval"
interpretVal (EVar (Var label val)) = reportError "Not yet implemented EVar"
interpretVal (ELitInt n) = return $ VInt n
interpretVal  ELitTrue = return $ VBool True
interpretVal  ELitFalse = return $ VBool False
interpretVal (EString str) = return $ VString str
interpretVal (EApp (Ident funName) args) = if isBuiltIn funName
    then executeBuiltIn funName args
    else reportError "Not yet implemented EApp"
interpretVal (EArr arr) = reportError "Not yet implemented EArr"
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

interpretVal (EAnd lhs rhs) = reportError "Not yet implemented EAnd"
interpretVal (EOr lhs rhs) = reportError "Not yet implemented EOr"
interpretVal (ECase val cases) = reportError "Not yet implemented ECase"

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
    
runInterp :: Interp () -> Result String
runInterp m = fmap (output . snd) $ runStateT m emptyExecState

emptyExecState :: ExecState
emptyExecState = ExecState {vEnv = [], store = [], output = ""}

usage :: IO ()
usage = putStrLn "usage: ./interpreter program"
